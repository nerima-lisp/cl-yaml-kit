;;;; t/loader-schema-test.lisp
(in-package #:cl-yaml-kit/test)

(describe "loader schema"
  (loader-scalar-cases
    ("core null" :core "" "tag:yaml.org,2002:null")
    ("core octal" :core "0o17" "tag:yaml.org,2002:int")
    ("core float" :core "1.25" "tag:yaml.org,2002:float")
    ("core bool" :core "true" "tag:yaml.org,2002:bool")
    ("json float" :json "1.25" "tag:yaml.org,2002:float")
    ("json trailing dot" :json "1." "tag:yaml.org,2002:str")
    ("json exponent trailing dot" :json "1.e2" "tag:yaml.org,2002:str")
    ("json leading zero" :json "01" "tag:yaml.org,2002:str")
    ("failsafe scalar" :failsafe "true" "tag:yaml.org,2002:str")
    ("core underscore scalar" :core "1_000" "tag:yaml.org,2002:str")
    ("core hexadecimal" :core "0x10" "tag:yaml.org,2002:int")
    ("core nan" :core ".NaN" "tag:yaml.org,2002:float")
    ("core positive infinity" :core ".inf" "tag:yaml.org,2002:float")
    ("core negative infinity" :core "-.Inf" "tag:yaml.org,2002:float")
    ("json null" :json "null" "tag:yaml.org,2002:null")
    ("json boolean" :json "false" "tag:yaml.org,2002:bool")
    ("schema fast path" :core "#" "tag:yaml.org,2002:str"))

  (loader-parse-cases
    ("signed octal" "-0o17" () -15)
    ("signed hexadecimal" "+0x10" () 16)
    ("decimal float" "0.278" () 0.278d0)
    ("integral decimal float" "450.00" () 450)
    ("positive infinity" ".INF" () sb-kernel::double-float-positive-infinity)
    ("negative infinity" "-.INF" () sb-kernel::double-float-negative-infinity))

  (loader-value-cases
    ("empty input has no documents"
     (loader-parse-all-events nil)
     nil)
    ("empty sequence can be a list"
     (loader-parse-events
      (loader-empty-collection-events :sequence nil)
      :sequence-type :list)
     nil))

  (loader-value-cases
    ("explicit string tag resolves"
     (yaml-kit::resolve-tag
      (yaml-kit:make-scalar-node :value "true" :tag "!!str" :style :plain))
     "tag:yaml.org,2002:str")
    ("explicit integer tag resolves"
     (yaml-kit::resolve-tag
      (yaml-kit:make-scalar-node :value "1" :tag "!!int" :style :plain))
     "tag:yaml.org,2002:int")
    ("explicit float tag resolves"
     (yaml-kit::resolve-tag
      (yaml-kit:make-scalar-node :value "1.0" :tag "!!float" :style :plain))
     "tag:yaml.org,2002:float")
    ("explicit boolean tag resolves"
     (yaml-kit::resolve-tag
      (yaml-kit:make-scalar-node :value "true" :tag "!!bool" :style :plain))
     "tag:yaml.org,2002:bool")
    ("explicit null tag resolves"
     (yaml-kit::resolve-tag
      (yaml-kit:make-scalar-node :value "null" :tag "!!null" :style :plain))
     "tag:yaml.org,2002:null")
    ("explicit sequence tag resolves"
     (yaml-kit::resolve-tag (yaml-kit:make-sequence-node :tag "!!seq"))
     "tag:yaml.org,2002:seq")
    ("explicit mapping tag resolves"
     (yaml-kit::resolve-tag (yaml-kit:make-mapping-node :tag "!!map"))
     "tag:yaml.org,2002:map")
    ("non-specific scalar tag resolves as string"
     (yaml-kit::resolve-tag
      (yaml-kit:make-scalar-node :value "true" :tag "!" :style :plain))
     "tag:yaml.org,2002:str")
    ("non-specific tag resolves by schema"
     (yaml-kit::resolve-tag
     (yaml-kit:make-scalar-node :value "true" :tag "?" :style :plain)
      :schema :core)
     "tag:yaml.org,2002:bool")))
