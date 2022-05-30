(() => {
  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };
  var compose = function(dict) {
    return dict.compose;
  };
  var composeFlipped = function(dictSemigroupoid) {
    return function(f) {
      return function(g) {
        return compose(dictSemigroupoid)(g)(f);
      };
    };
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b2) {
      return function(a2) {
        return f(a2)(b2);
      };
    };
  };
  var $$const = function(a2) {
    return function(v) {
      return a2;
    };
  };
  var applyFlipped = function(x) {
    return function(f) {
      return f(x);
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(arr[i2]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    return function(fa) {
      return function(f) {
        return map(dictFunctor)(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    return function(f) {
      return function(x) {
        return map(dictFunctor)($$const(x))(f);
      };
    };
  };
  var voidRight = function(dictFunctor) {
    return function(x) {
      return map(dictFunctor)($$const(x));
    };
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    return function(a2) {
      return function(b2) {
        return apply(dictApply)(map(dictApply.Functor0())($$const(identity(categoryFn)))(a2))(b2);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure(dictApplicative)(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure(dictApplicative)(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    return function(f) {
      return function(a2) {
        return apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a2);
      };
    };
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisliFlipped = function(dictBind) {
    return function(f) {
      return function(g) {
        return function(a2) {
          return bindFlipped(dictBind)(f)(g(a2));
        };
      };
    };
  };
  var composeKleisli = function(dictBind) {
    return function(f) {
      return function(g) {
        return function(a2) {
          return bind(dictBind)(f(a2))(g);
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };
  var join = function(dictBind) {
    return function(m) {
      return bind(dictBind)(m)(identity(categoryFn));
    };
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq2) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq2 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqBooleanImpl = refEq;
  var eqIntImpl = refEq;
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeHas = function(label5) {
    return function(rec) {
      return {}.hasOwnProperty.call(rec, label5);
    };
  };
  var unsafeGet = function(label5) {
    return function(rec) {
      return rec[label5];
    };
  };
  var unsafeSet = function(label5) {
    return function(value13) {
      return function(rec) {
        var copy2 = {};
        for (var key in rec) {
          if ({}.hasOwnProperty.call(rec, key)) {
            copy2[key] = rec[key];
          }
        }
        copy2[label5] = value13;
        return copy2;
      };
    };
  };
  var unsafeDelete = function(label5) {
    return function(rec) {
      var copy2 = {};
      for (var key in rec) {
        if (key !== label5 && {}.hasOwnProperty.call(rec, key)) {
          copy2[key] = rec[key];
        }
      }
      return copy2;
    };
  };

  // output/Data.Eq/index.js
  var eqUnit = {
    eq: function(v) {
      return function(v1) {
        return true;
      };
    }
  };
  var eqString = {
    eq: eqStringImpl
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eqBoolean = {
    eq: eqBooleanImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };
  var notEq = function(dictEq) {
    return function(x) {
      return function(y) {
        return eq(eqBoolean)(eq(dictEq)(x)(y))(false);
      };
    };
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
    };
  };

  // output/Data.Semiring/index.js
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };

  // output/Data.Ring/index.js
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };

  // output/Data.Ord/index.js
  var ordUnit = {
    compare: function(v) {
      return function(v1) {
        return EQ.value;
      };
    },
    Eq0: function() {
      return eqUnit;
    }
  };
  var ordString = /* @__PURE__ */ function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  }();
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var ordChar = /* @__PURE__ */ function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  }();
  var compare = function(dict) {
    return dict.compare;
  };
  var comparing = function(dictOrd) {
    return function(f) {
      return function(x) {
        return function(y) {
          return compare(dictOrd)(f(x))(f(y));
        };
      };
    };
  };

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showNumberImpl = function(n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(/[\0-\x1F\x7F"\\]/g, function(c, i2) {
      switch (c) {
        case '"':
        case "\\":
          return "\\" + c;
        case "\x07":
          return "\\a";
        case "\b":
          return "\\b";
        case "\f":
          return "\\f";
        case "\n":
          return "\\n";
        case "\r":
          return "\\r";
        case "	":
          return "\\t";
        case "\v":
          return "\\v";
      }
      var k = i2 + 1;
      var empty7 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
      return "\\" + c.charCodeAt(0).toString(10) + empty7;
    }) + '"';
  };
  var cons = function(head4) {
    return function(tail2) {
      return [head4].concat(tail2);
    };
  };
  var intercalate = function(separator) {
    return function(xs) {
      return xs.join(separator);
    };
  };

  // output/Data.Show/index.js
  var showString = {
    show: showStringImpl
  };
  var showRecordFieldsNil = {
    showRecordFields: function(v) {
      return function(v1) {
        return [];
      };
    }
  };
  var showRecordFields = function(dict) {
    return dict.showRecordFields;
  };
  var showRecord = function() {
    return function() {
      return function(dictShowRecordFields) {
        return {
          show: function(record) {
            var v = showRecordFields(dictShowRecordFields)($$Proxy.value)(record);
            if (v.length === 0) {
              return "{}";
            }
            ;
            return intercalate(" ")(["{", intercalate(", ")(v), "}"]);
          }
        };
      };
    };
  };
  var showNumber = {
    show: showNumberImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var showBoolean = {
    show: function(v) {
      if (v) {
        return "true";
      }
      ;
      if (!v) {
        return "false";
      }
      ;
      throw new Error("Failed pattern match at Data.Show (line 23, column 1 - line 25, column 23): " + [v.constructor.name]);
    }
  };
  var show = function(dict) {
    return dict.show;
  };
  var showRecordFieldsCons = function(dictIsSymbol) {
    return function(dictShowRecordFields) {
      return function(dictShow) {
        return {
          showRecordFields: function(v) {
            return function(record) {
              var tail2 = showRecordFields(dictShowRecordFields)($$Proxy.value)(record);
              var key = reflectSymbol(dictIsSymbol)($$Proxy.value);
              var focus3 = unsafeGet(key)(record);
              return cons(intercalate(": ")([key, show(dictShow)(focus3)]))(tail2);
            };
          }
        };
      };
    };
  };

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b2) {
    return !b2;
  };

  // output/Data.HeytingAlgebra/index.js
  var tt = function(dict) {
    return dict.tt;
  };
  var not = function(dict) {
    return dict.not;
  };
  var implies = function(dict) {
    return dict.implies;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a2) {
      return function(b2) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a2))(b2);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };
  var conj = function(dict) {
    return dict.conj;
  };
  var heytingAlgebraFunction = function(dictHeytingAlgebra) {
    return {
      ff: function(v) {
        return ff(dictHeytingAlgebra);
      },
      tt: function(v) {
        return tt(dictHeytingAlgebra);
      },
      implies: function(f) {
        return function(g) {
          return function(a2) {
            return implies(dictHeytingAlgebra)(f(a2))(g(a2));
          };
        };
      },
      conj: function(f) {
        return function(g) {
          return function(a2) {
            return conj(dictHeytingAlgebra)(f(a2))(g(a2));
          };
        };
      },
      disj: function(f) {
        return function(g) {
          return function(a2) {
            return disj(dictHeytingAlgebra)(f(a2))(g(a2));
          };
        };
      },
      not: function(f) {
        return function(a2) {
          return not(dictHeytingAlgebra)(f(a2));
        };
      }
    };
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Void/index.js
  var absurd = function(a2) {
    var spin = function($copy_v) {
      var $tco_result;
      function $tco_loop(v) {
        $copy_v = v;
        return;
      }
      ;
      while (true) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    return spin(a2);
  };

  // output/Data.Semigroup/index.js
  var semigroupString = {
    append: concatString
  };
  var semigroupRecordNil = {
    appendRecord: function(v) {
      return function(v1) {
        return function(v2) {
          return {};
        };
      };
    }
  };
  var semigroupArray = {
    append: concatArray
  };
  var appendRecord = function(dict) {
    return dict.appendRecord;
  };
  var semigroupRecord = function() {
    return function(dictSemigroupRecord) {
      return {
        append: appendRecord(dictSemigroupRecord)($$Proxy.value)
      };
    };
  };
  var append = function(dict) {
    return dict.append;
  };
  var semigroupRecordCons = function(dictIsSymbol) {
    return function() {
      return function(dictSemigroupRecord) {
        return function(dictSemigroup) {
          return {
            appendRecord: function(v) {
              return function(ra) {
                return function(rb) {
                  var tail2 = appendRecord(dictSemigroupRecord)($$Proxy.value)(ra)(rb);
                  var key = reflectSymbol(dictIsSymbol)($$Proxy.value);
                  var insert7 = unsafeSet(key);
                  var get3 = unsafeGet(key);
                  return insert7(append(dictSemigroup)(get3(ra))(get3(rb)))(tail2);
                };
              };
            }
          };
        };
      };
    };
  };

  // output/Data.Monoid/index.js
  var monoidString = {
    mempty: "",
    Semigroup0: function() {
      return semigroupString;
    }
  };
  var monoidRecordNil = {
    memptyRecord: function(v) {
      return {};
    },
    SemigroupRecord0: function() {
      return semigroupRecordNil;
    }
  };
  var monoidArray = {
    mempty: [],
    Semigroup0: function() {
      return semigroupArray;
    }
  };
  var memptyRecord = function(dict) {
    return dict.memptyRecord;
  };
  var monoidRecord = function() {
    return function(dictMonoidRecord) {
      return {
        mempty: memptyRecord(dictMonoidRecord)($$Proxy.value),
        Semigroup0: function() {
          return semigroupRecord()(dictMonoidRecord.SemigroupRecord0());
        }
      };
    };
  };
  var mempty = function(dict) {
    return dict.mempty;
  };
  var monoidRecordCons = function(dictIsSymbol) {
    return function(dictMonoid) {
      return function() {
        return function(dictMonoidRecord) {
          return {
            memptyRecord: function(v) {
              var tail2 = memptyRecord(dictMonoidRecord)($$Proxy.value);
              var key = reflectSymbol(dictIsSymbol)($$Proxy.value);
              var insert7 = unsafeSet(key);
              return insert7(mempty(dictMonoid))(tail2);
            },
            SemigroupRecord0: function() {
              return semigroupRecordCons(dictIsSymbol)()(dictMonoidRecord.SemigroupRecord0())(dictMonoid.Semigroup0());
            }
          };
        };
      };
    };
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var snd = function(v) {
    return v.value1;
  };
  var functorTuple = {
    map: function(f) {
      return function(m) {
        return new Tuple(m.value0, f(m.value1));
      };
    }
  };
  var fst = function(v) {
    return v.value0;
  };
  var eqTuple = function(dictEq) {
    return function(dictEq1) {
      return {
        eq: function(x) {
          return function(y) {
            return eq(dictEq)(x.value0)(y.value0) && eq(dictEq1)(x.value1)(y.value1);
          };
        }
      };
    };
  };
  var ordTuple = function(dictOrd) {
    return function(dictOrd1) {
      return {
        compare: function(x) {
          return function(y) {
            var v = compare(dictOrd)(x.value0)(y.value0);
            if (v instanceof LT) {
              return LT.value;
            }
            ;
            if (v instanceof GT) {
              return GT.value;
            }
            ;
            return compare(dictOrd1)(x.value1)(y.value1);
          };
        },
        Eq0: function() {
          return eqTuple(dictOrd.Eq0())(dictOrd1.Eq0());
        }
      };
    };
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var put = function(dictMonadState) {
    return function(s) {
      return state(dictMonadState)(function(v) {
        return new Tuple(unit, s);
      });
    };
  };
  var modify_ = function(dictMonadState) {
    return function(f) {
      return state(dictMonadState)(function(s) {
        return new Tuple(unit, f(s));
      });
    };
  };
  var modify = function(dictMonadState) {
    return function(f) {
      return state(dictMonadState)(function(s) {
        var s$prime = f(s);
        return new Tuple(s$prime, s$prime);
      });
    };
  };
  var gets = function(dictMonadState) {
    return function(f) {
      return state(dictMonadState)(function(s) {
        return new Tuple(f(s), s);
      });
    };
  };
  var get = function(dictMonadState) {
    return state(dictMonadState)(function(s) {
      return new Tuple(s, s);
    });
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = len - 1; i2 >= 0; i2--) {
          acc = f(xs[i2])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = 0; i2 < len; i2++) {
          acc = f(acc)(xs[i2]);
        }
        return acc;
      };
    };
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Data.Maybe/index.js
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var fromMaybe = function(a2) {
    return maybe(a2)(identity(categoryFn));
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map(functorMaybe)(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };
  var applicativeMaybe = /* @__PURE__ */ function() {
    return {
      pure: Just.create,
      Apply0: function() {
        return applyMaybe;
      }
    };
  }();

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var note = function(a2) {
    return maybe(new Left(a2))(Right.create);
  };
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var hush = /* @__PURE__ */ function() {
    return either($$const(Nothing.value))(Just.create);
  }();
  var isRight = /* @__PURE__ */ either(/* @__PURE__ */ $$const(false))(/* @__PURE__ */ $$const(true));
  var applyEither = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Left) {
          return new Left(v.value0);
        }
        ;
        if (v instanceof Right) {
          return map(functorEither)(v.value0)(v1);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorEither;
    }
  };
  var bindEither = {
    bind: /* @__PURE__ */ either(function(e) {
      return function(v) {
        return new Left(e);
      };
    })(function(a2) {
      return function(f) {
        return f(a2);
      };
    }),
    Apply0: function() {
      return applyEither;
    }
  };
  var applicativeEither = /* @__PURE__ */ function() {
    return {
      pure: Right.create,
      Apply0: function() {
        return applyEither;
      }
    };
  }();

  // output/Data.Bifunctor/index.js
  var bimap = function(dict) {
    return dict.bimap;
  };
  var lmap = function(dictBifunctor) {
    return function(f) {
      return bimap(dictBifunctor)(f)(identity(categoryFn));
    };
  };
  var bifunctorEither = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Left) {
            return new Left(v(v2.value0));
          }
          ;
          if (v2 instanceof Right) {
            return new Right(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.Bifunctor (line 32, column 1 - line 34, column 36): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var unwrap = coerce;

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    return function(dictFoldable) {
      return function(f) {
        return foldr(dictFoldable)(function() {
          var $316 = applySecond(dictApplicative.Apply0());
          return function($317) {
            return $316(f($317));
          };
        }())(pure(dictApplicative)(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    return function(dictFoldable) {
      return flip(traverse_(dictApplicative)(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var indexl = function(dictFoldable) {
    return function(idx) {
      var go2 = function(cursor) {
        return function(a2) {
          if (cursor.elem instanceof Just) {
            return cursor;
          }
          ;
          var $157 = cursor.pos === idx;
          if ($157) {
            return {
              elem: new Just(a2),
              pos: cursor.pos
            };
          }
          ;
          return {
            pos: cursor.pos + 1 | 0,
            elem: cursor.elem
          };
        };
      };
      var $318 = foldl(dictFoldable)(go2)({
        elem: Nothing.value,
        pos: 0
      });
      return function($319) {
        return function(v) {
          return v.elem;
        }($318($319));
      };
    };
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(z) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return z;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0)(z);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(z) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return z;
          }
          ;
          if (v1 instanceof Just) {
            return v(z)(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, z.constructor.name, v1.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty(dictMonoid);
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    return function(dictMonoid) {
      return function(f) {
        return foldr(dictFoldable)(function(x) {
          return function(acc) {
            return append(dictMonoid.Semigroup0())(f(x))(acc);
          };
        })(mempty(dictMonoid));
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMapDefaultL = function(dictFoldable) {
    return function(dictMonoid) {
      return function(f) {
        return foldl(dictFoldable)(function(acc) {
          return function(x) {
            return append(dictMonoid.Semigroup0())(acc)(f(x));
          };
        })(mempty(dictMonoid));
      };
    };
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };

  // output/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error4) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error4) {
        setTimeout(function() {
          throw error4;
        }, 0);
      }
    }
    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error4) {
        return left(error4);
      }
    }
    function runAsync(left, eff, k) {
      try {
        return eff(k)();
      } catch (error4) {
        k(left(error4))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size7 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size7 !== 0) {
          size7--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i2, tmp;
          if (size7 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size7) % limit] = cb;
          size7++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill2(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function() {
                      throw util.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill2(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error4) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step4 = aff;
      var fail2 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run3(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step4 = bhead(step4);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail2 = util.left(e);
                step4 = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step4)) {
                status = RETURN;
                fail2 = step4;
                step4 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step4 = util.fromRight(step4);
              }
              break;
            case CONTINUE:
              switch (step4.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step4._2;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step4 = util.right(step4._1);
                  } else {
                    status = STEP_BIND;
                    step4 = step4._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step4 = runSync(util.left, util.right, step4._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step4 = runAsync(util.left, step4._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step4 = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail2 = util.left(step4._1);
                  step4 = null;
                  break;
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step4, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step4, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step4._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step4._1) {
                    tmp.run();
                  }
                  step4 = util.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step4 = sequential2(util, supervisor, step4._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step4 = interrupt || fail2 || step4;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail2) {
                      status = CONTINUE;
                      step4 = attempt._2(util.fromLeft(fail2));
                      fail2 = null;
                    }
                    break;
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail2) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step4 = util.fromRight(step4);
                    }
                    break;
                  case BRACKET:
                    bracketCount--;
                    if (fail2 === null) {
                      result = util.fromRight(step4);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step4 = attempt._3(result);
                      }
                    }
                    break;
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail2), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step4 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail2) {
                      step4 = attempt._1.failed(util.fromLeft(fail2))(attempt._2);
                    } else {
                      step4 = attempt._1.completed(util.fromRight(step4))(attempt._2);
                    }
                    fail2 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail2), attempts, interrupt);
                    status = CONTINUE;
                    step4 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step4 = attempt._1;
                    fail2 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step4));
                }
              }
              joins = null;
              if (interrupt && fail2) {
                setTimeout(function() {
                  throw util.fromLeft(fail2);
                }, 0);
              } else if (util.isLeft(step4) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step4);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join4) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join4.rethrow;
            join4.handler(step4)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join4;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill2(error4, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error4);
              status = COMPLETED;
              step4 = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step4(error4)), attempts, interrupt);
                }
                status = RETURN;
                step4 = null;
                fail2 = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step4 = null;
                fail2 = null;
              }
          }
          return canceler;
        };
      }
      function join3(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run3(runTick);
          }
          return canceler;
        };
      }
      return {
        kill: kill2,
        join: join3,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run3(runTick);
              });
            } else {
              run3(runTick);
            }
          }
        }
      };
    }
    function runPar(util, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill2(error4, par2, cb2) {
        var step4 = par2;
        var head4 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop:
          while (true) {
            tmp = null;
            switch (step4.tag) {
              case FORKED:
                if (step4._3 === EMPTY) {
                  tmp = fibers[step4._1];
                  kills2[count++] = tmp.kill(error4, function(result) {
                    return function() {
                      count--;
                      if (count === 0) {
                        cb2(result)();
                      }
                    };
                  });
                }
                if (head4 === null) {
                  break loop;
                }
                step4 = head4._2;
                if (tail2 === null) {
                  head4 = null;
                } else {
                  head4 = tail2._1;
                  tail2 = tail2._2;
                }
                break;
              case MAP:
                step4 = step4._2;
                break;
              case APPLY:
              case ALT:
                if (head4) {
                  tail2 = new Aff2(CONS, head4, tail2);
                }
                head4 = step4;
                step4 = step4._1;
                break;
            }
          }
        if (count === 0) {
          cb2(util.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join3(result, head4, tail2) {
        var fail2, step4, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail2 = result;
          step4 = null;
        } else {
          step4 = result;
          fail2 = null;
        }
        loop:
          while (true) {
            lhs = null;
            rhs = null;
            tmp = null;
            kid = null;
            if (interrupt !== null) {
              return;
            }
            if (head4 === null) {
              cb(fail2 || step4)();
              return;
            }
            if (head4._3 !== EMPTY) {
              return;
            }
            switch (head4.tag) {
              case MAP:
                if (fail2 === null) {
                  head4._3 = util.right(head4._1(util.fromRight(step4)));
                  step4 = head4._3;
                } else {
                  head4._3 = fail2;
                }
                break;
              case APPLY:
                lhs = head4._1._3;
                rhs = head4._2._3;
                if (fail2) {
                  head4._3 = fail2;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, fail2 === lhs ? head4._2 : head4._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join3(fail2, null, null);
                      } else {
                        join3(fail2, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                } else if (lhs === EMPTY || rhs === EMPTY) {
                  return;
                } else {
                  step4 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                  head4._3 = step4;
                }
                break;
              case ALT:
                lhs = head4._1._3;
                rhs = head4._2._3;
                if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                  return;
                }
                if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                  fail2 = step4 === lhs ? rhs : lhs;
                  step4 = null;
                  head4._3 = fail2;
                } else {
                  head4._3 = step4;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, step4 === lhs ? head4._2 : head4._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join3(step4, null, null);
                      } else {
                        join3(step4, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                }
                break;
            }
            if (tail2 === null) {
              head4 = null;
            } else {
              head4 = tail2._1;
              tail2 = tail2._2;
            }
          }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join3(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run3() {
        var status = CONTINUE;
        var step4 = par;
        var head4 = null;
        var tail2 = null;
        var tmp, fid;
        loop:
          while (true) {
            tmp = null;
            fid = null;
            switch (status) {
              case CONTINUE:
                switch (step4.tag) {
                  case MAP:
                    if (head4) {
                      tail2 = new Aff2(CONS, head4, tail2);
                    }
                    head4 = new Aff2(MAP, step4._1, EMPTY, EMPTY);
                    step4 = step4._2;
                    break;
                  case APPLY:
                    if (head4) {
                      tail2 = new Aff2(CONS, head4, tail2);
                    }
                    head4 = new Aff2(APPLY, EMPTY, step4._2, EMPTY);
                    step4 = step4._1;
                    break;
                  case ALT:
                    if (head4) {
                      tail2 = new Aff2(CONS, head4, tail2);
                    }
                    head4 = new Aff2(ALT, EMPTY, step4._2, EMPTY);
                    step4 = step4._1;
                    break;
                  default:
                    fid = fiberId++;
                    status = RETURN;
                    tmp = step4;
                    step4 = new Aff2(FORKED, fid, new Aff2(CONS, head4, tail2), EMPTY);
                    tmp = Fiber(util, supervisor, tmp);
                    tmp.onComplete({
                      rethrow: false,
                      handler: resolve(step4)
                    })();
                    fibers[fid] = tmp;
                    if (supervisor) {
                      supervisor.register(tmp);
                    }
                }
                break;
              case RETURN:
                if (head4 === null) {
                  break loop;
                }
                if (head4._1 === EMPTY) {
                  head4._1 = step4;
                  status = CONTINUE;
                  step4 = head4._2;
                  head4._2 = EMPTY;
                } else {
                  head4._2 = step4;
                  step4 = head4;
                  if (tail2 === null) {
                    head4 = null;
                  } else {
                    head4 = tail2._1;
                    tail2 = tail2._2;
                  }
                }
            }
          }
        root = step4;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error4, cb2) {
        interrupt = util.left(error4);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill2(error4, root, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run3();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential2(util, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value13) {
          return Aff.Pure(f(value13));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  function _fork(immediate) {
    return function(aff) {
      return Aff.Fork(immediate, aff);
    };
  }
  var _liftEffect = Aff.Sync;
  function _parAffMap(f) {
    return function(aff) {
      return Aff.ParMap(f, aff);
    };
  }
  function _parAffApply(aff1) {
    return function(aff2) {
      return Aff.ParApply(aff1, aff2);
    };
  }
  var makeAff = Aff.Async;
  function generalBracket(acquire) {
    return function(options2) {
      return function(k) {
        return Aff.Bracket(acquire, options2, k);
      };
    };
  }
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var _delay = function() {
    function setDelay(n, k) {
      if (n === 0 && typeof setImmediate !== "undefined") {
        return setImmediate(k);
      } else {
        return setTimeout(k, n);
      }
    }
    function clearDelay(n, t) {
      if (n === 0 && typeof clearImmediate !== "undefined") {
        return clearImmediate(t);
      } else {
        return clearTimeout(t);
      }
    }
    return function(right, ms) {
      return Aff.Async(function(cb) {
        return function() {
          var timer = setDelay(ms, cb(right()));
          return function() {
            return Aff.Sync(function() {
              return right(clearDelay(ms, timer));
            });
          };
        };
      });
    };
  }();
  var _sequential = Aff.Seq;

  // output/Control.Monad/index.js
  var unlessM = function(dictMonad) {
    return function(mb) {
      return function(m) {
        return bind(dictMonad.Bind1())(mb)(function(b2) {
          return unless(dictMonad.Applicative0())(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    return function(f) {
      return function(a2) {
        return bind(dictMonad.Bind1())(f)(function(f$prime) {
          return bind(dictMonad.Bind1())(a2)(function(a$prime) {
            return pure(dictMonad.Applicative0())(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Effect/foreign.js
  var pureE = function(a2) {
    return function() {
      return a2;
    };
  };
  var bindE = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);
  var applyEffect = /* @__PURE__ */ $lazy_applyEffect(23);

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($2) {
    return throwException(error($2));
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    return function(a2) {
      return catchError(dictMonadError)(map(dictMonadError.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(Right.create)(a2))(function() {
        var $21 = pure(dictMonadError.MonadThrow0().Monad0().Applicative0());
        return function($22) {
          return $21(Left.create($22));
        };
      }());
    };
  };

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref2) {
    return function() {
      return ref2.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref2) {
      return function() {
        var t = f(ref2.value);
        ref2.value = t.state;
        return t.value;
      };
    };
  };
  var write = function(val) {
    return function(ref2) {
      return function() {
        ref2.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$new = _new;
  var modify$prime = modifyImpl;
  var modify2 = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var modify_2 = function(f) {
    return function(s) {
      return $$void(functorEffect)(modify2(f)(s));
    };
  };

  // output/Control.Monad.Rec.Class/index.js
  var Loop = /* @__PURE__ */ function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  }();
  var Done = /* @__PURE__ */ function() {
    function Done2(value0) {
      this.value0 = value0;
    }
    ;
    Done2.create = function(value0) {
      return new Done2(value0);
    };
    return Done2;
  }();
  var tailRecM = function(dict) {
    return dict.tailRecM;
  };
  var monadRecEffect = {
    tailRecM: function(f) {
      return function(a2) {
        var fromDone = function(v) {
          if (v instanceof Done) {
            return v.value0;
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 113, column 30 - line 113, column 44): " + [v.constructor.name]);
        };
        return function __do2() {
          var r = bindFlipped(bindEffect)($$new)(f(a2))();
          (function() {
            while (!function __do3() {
              var v = read(r)();
              if (v instanceof Loop) {
                var e = f(v.value0)();
                write(e)(r)();
                return false;
              }
              ;
              if (v instanceof Done) {
                return true;
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 104, column 22 - line 109, column 28): " + [v.constructor.name]);
            }()) {
            }
            ;
            return {};
          })();
          return map(functorEffect)(fromDone)(read(r))();
        };
      };
    },
    Monad0: function() {
      return monadEffect;
    }
  };

  // output/Control.Monad.Trans.Class/index.js
  var lift = function(dict) {
    return dict.lift;
  };

  // output/Effect.Class/index.js
  var monadEffectEffect = {
    liftEffect: /* @__PURE__ */ identity(categoryFn),
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.Except.Trans/index.js
  var ExceptT = function(x) {
    return x;
  };
  var runExceptT = function(v) {
    return v;
  };
  var monadTransExceptT = {
    lift: function(dictMonad) {
      return function(m) {
        return bind(dictMonad.Bind1())(m)(function(a2) {
          return pure(dictMonad.Applicative0())(new Right(a2));
        });
      };
    }
  };
  var mapExceptT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorExceptT = function(dictFunctor) {
    return {
      map: function(f) {
        return mapExceptT(map(dictFunctor)(map(functorEither)(f)));
      }
    };
  };
  var except = function(dictApplicative) {
    var $87 = pure(dictApplicative);
    return function($88) {
      return ExceptT($87($88));
    };
  };
  var monadExceptT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeExceptT(dictMonad);
      },
      Bind1: function() {
        return bindExceptT(dictMonad);
      }
    };
  };
  var bindExceptT = function(dictMonad) {
    return {
      bind: function(v) {
        return function(k) {
          return bind(dictMonad.Bind1())(v)(either(function() {
            var $89 = pure(dictMonad.Applicative0());
            return function($90) {
              return $89(Left.create($90));
            };
          }())(function(a2) {
            var v1 = k(a2);
            return v1;
          }));
        };
      },
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var applyExceptT = function(dictMonad) {
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT(dictMonad.Bind1().Apply0().Functor0());
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: function() {
        var $91 = pure(dictMonad.Applicative0());
        return function($92) {
          return ExceptT($91(Right.create($92)));
        };
      }(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    return {
      throwError: function() {
        var $101 = pure(dictMonad.Applicative0());
        return function($102) {
          return ExceptT($101(Left.create($102)));
        };
      }(),
      Monad0: function() {
        return monadExceptT(dictMonad);
      }
    };
  };

  // output/Type.Equality/index.js
  var refl = {
    proof: function(a2) {
      return a2;
    },
    Coercible0: function() {
      return void 0;
    }
  };
  var proof = function(dict) {
    return dict.proof;
  };
  var from = function(dictTypeEquals) {
    var v = proof(dictTypeEquals)(function(a2) {
      return a2;
    });
    return v;
  };

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a2) {
      return [a2];
    }
    function array2(a2) {
      return function(b2) {
        return [a2, b2];
      };
    }
    function array3(a2) {
      return function(b2) {
        return function(c) {
          return [a2, b2, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply2) {
      return function(map3) {
        return function(pure2) {
          return function(f) {
            return function(array) {
              function go2(bot, top2) {
                switch (top2 - bot) {
                  case 0:
                    return pure2([]);
                  case 1:
                    return map3(array1)(f(array[bot]));
                  case 2:
                    return apply2(map3(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply2(apply2(map3(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                    return apply2(map3(concat2)(go2(bot, pivot)))(go2(pivot, top2));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Control.Parallel/index.js
  var parTraverse_ = function(dictParallel) {
    return function(dictFoldable) {
      return function(f) {
        var $17 = sequential(dictParallel);
        var $18 = traverse_(dictParallel.Applicative1())(dictFoldable)(function() {
          var $20 = parallel(dictParallel);
          return function($21) {
            return $20(f($21));
          };
        }());
        return function($19) {
          return $17($18($19));
        };
      };
    };
  };
  var parSequence_ = function(dictParallel) {
    return function(dictFoldable) {
      return parTraverse_(dictParallel)(dictFoldable)(identity(categoryFn));
    };
  };

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith()(msg);
    });
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy2 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Canceler = function(x) {
    return x;
  };
  var suspendAff = /* @__PURE__ */ _fork(false);
  var functorParAff = {
    map: _parAffMap
  };
  var functorAff = {
    map: _map
  };
  var forkAff = /* @__PURE__ */ _fork(true);
  var ffiUtil = /* @__PURE__ */ function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  }();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do2() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var launchAff_ = /* @__PURE__ */ function() {
    var $39 = $$void(functorEffect);
    return function($40) {
      return $39(launchAff($40));
    };
  }();
  var delay = function(v) {
    return _delay(Right.create, v);
  };
  var bracket = function(acquire) {
    return function(completed) {
      return generalBracket(acquire)({
        killed: $$const(completed),
        failed: $$const(completed),
        completed: $$const(completed)
      });
    };
  };
  var applyParAff = {
    apply: _parAffApply,
    Functor0: function() {
      return functorParAff;
    }
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy2("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var $$finally = function(fin) {
    return function(a2) {
      return bracket(pure(applicativeAff)(unit))($$const(fin))($$const(a2));
    };
  };
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var effectCanceler = /* @__PURE__ */ function() {
    var $41 = liftEffect(monadEffectAff);
    return function($42) {
      return Canceler($$const($41($42)));
    };
  }();
  var joinFiber = function(v) {
    return makeAff(function(k) {
      return map(functorEffect)(effectCanceler)(v.join(k));
    });
  };
  var functorFiber = {
    map: function(f) {
      return function(t) {
        return unsafePerformEffect(makeFiber(map(functorAff)(f)(joinFiber(t))));
      };
    }
  };
  var killFiber = function(e) {
    return function(v) {
      return bind(bindAff)(liftEffect(monadEffectAff)(v.isSuspended))(function(suspended) {
        if (suspended) {
          return liftEffect(monadEffectAff)($$void(functorEffect)(v.kill(e, $$const(pure(applicativeEffect)(unit)))));
        }
        ;
        return makeAff(function(k) {
          return map(functorEffect)(effectCanceler)(v.kill(e, k));
        });
      });
    };
  };
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var runAff = function(k) {
    return function(aff) {
      return launchAff(bindFlipped(bindAff)(function() {
        var $45 = liftEffect(monadEffectAff);
        return function($46) {
          return $45(k($46));
        };
      }())($$try(monadErrorAff)(aff)));
    };
  };
  var runAff_ = function(k) {
    return function(aff) {
      return $$void(functorEffect)(runAff(k)(aff));
    };
  };
  var parallelAff = {
    parallel: unsafeCoerce2,
    sequential: _sequential,
    Monad0: function() {
      return monadAff;
    },
    Applicative1: function() {
      return $lazy_applicativeParAff(0);
    }
  };
  var $lazy_applicativeParAff = /* @__PURE__ */ $runtime_lazy2("applicativeParAff", "Effect.Aff", function() {
    return {
      pure: function() {
        var $49 = parallel(parallelAff);
        var $50 = pure(applicativeAff);
        return function($51) {
          return $49($50($51));
        };
      }(),
      Apply0: function() {
        return applyParAff;
      }
    };
  });
  var applicativeParAff = /* @__PURE__ */ $lazy_applicativeParAff(131);
  var monadRecAff = {
    tailRecM: function(k) {
      var go2 = function(a2) {
        return bind(bindAff)(k(a2))(function(res) {
          if (res instanceof Done) {
            return pure(applicativeAff)(res.value0);
          }
          ;
          if (res instanceof Loop) {
            return go2(res.value0);
          }
          ;
          throw new Error("Failed pattern match at Effect.Aff (line 102, column 7 - line 104, column 23): " + [res.constructor.name]);
        });
      };
      return go2;
    },
    Monad0: function() {
      return monadAff;
    }
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeAff)(unit));

  // output/Record.Unsafe.Union/foreign.js
  function unsafeUnionFn(r1, r2) {
    var copy2 = {};
    for (var k1 in r2) {
      if ({}.hasOwnProperty.call(r2, k1)) {
        copy2[k1] = r2[k1];
      }
    }
    for (var k2 in r1) {
      if ({}.hasOwnProperty.call(r1, k2)) {
        copy2[k2] = r1[k2];
      }
    }
    return copy2;
  }

  // output/Data.Function.Uncurried/foreign.js
  var runFn3 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return fn(a2, b2, c);
        };
      };
    };
  };
  var runFn4 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return function(d) {
            return fn(a2, b2, c, d);
          };
        };
      };
    };
  };

  // output/Record/index.js
  var merge = function() {
    return function() {
      return function(l) {
        return function(r) {
          return unsafeUnionFn(l, r);
        };
      };
    };
  };
  var insert = function(dictIsSymbol) {
    return function() {
      return function() {
        return function(l) {
          return function(a2) {
            return function(r) {
              return unsafeSet(reflectSymbol(dictIsSymbol)(l))(a2)(r);
            };
          };
        };
      };
    };
  };
  var get2 = function(dictIsSymbol) {
    return function() {
      return function(l) {
        return function(r) {
          return unsafeGet(reflectSymbol(dictIsSymbol)(l))(r);
        };
      };
    };
  };
  var $$delete = function(dictIsSymbol) {
    return function() {
      return function() {
        return function(l) {
          return function(r) {
            return unsafeDelete(reflectSymbol(dictIsSymbol)(l))(r);
          };
        };
      };
    };
  };

  // output/Record.Builder/foreign.js
  function copyRecord(rec) {
    var copy2 = {};
    for (var key in rec) {
      if ({}.hasOwnProperty.call(rec, key)) {
        copy2[key] = rec[key];
      }
    }
    return copy2;
  }
  function unsafeInsert(l) {
    return function(a2) {
      return function(rec) {
        rec[l] = a2;
        return rec;
      };
    };
  }
  function unsafeModify(l) {
    return function(f) {
      return function(rec) {
        rec[l] = f(rec[l]);
        return rec;
      };
    };
  }

  // output/Record.Builder/index.js
  var semigroupoidBuilder = semigroupoidFn;
  var modify3 = function() {
    return function() {
      return function(dictIsSymbol) {
        return function(l) {
          return function(f) {
            return function(r1) {
              return unsafeModify(reflectSymbol(dictIsSymbol)(l))(f)(r1);
            };
          };
        };
      };
    };
  };
  var insert2 = function() {
    return function() {
      return function(dictIsSymbol) {
        return function(l) {
          return function(a2) {
            return function(r1) {
              return unsafeInsert(reflectSymbol(dictIsSymbol)(l))(a2)(r1);
            };
          };
        };
      };
    };
  };
  var categoryBuilder = categoryFn;
  var build = function(v) {
    return function(r1) {
      return v(copyRecord(r1));
    };
  };

  // output/ConvertableOptions/index.js
  var defaultsRecord = function() {
    return function() {
      return {
        defaults: flip(merge()())
      };
    };
  };
  var defaults = function(dict) {
    return dict.defaults;
  };

  // output/DOM.HTML.Indexed.ButtonType/index.js
  var ButtonButton = /* @__PURE__ */ function() {
    function ButtonButton2() {
    }
    ;
    ButtonButton2.value = new ButtonButton2();
    return ButtonButton2;
  }();
  var ButtonSubmit = /* @__PURE__ */ function() {
    function ButtonSubmit2() {
    }
    ;
    ButtonSubmit2.value = new ButtonSubmit2();
    return ButtonSubmit2;
  }();
  var ButtonReset = /* @__PURE__ */ function() {
    function ButtonReset2() {
    }
    ;
    ButtonReset2.value = new ButtonReset2();
    return ButtonReset2;
  }();
  var renderButtonType = function(v) {
    if (v instanceof ButtonButton) {
      return "button";
    }
    ;
    if (v instanceof ButtonSubmit) {
      return "submit";
    }
    ;
    if (v instanceof ButtonReset) {
      return "reset";
    }
    ;
    throw new Error("Failed pattern match at DOM.HTML.Indexed.ButtonType (line 14, column 20 - line 17, column 25): " + [v.constructor.name]);
  };

  // output/DOM.HTML.Indexed.InputType/index.js
  var InputButton = /* @__PURE__ */ function() {
    function InputButton2() {
    }
    ;
    InputButton2.value = new InputButton2();
    return InputButton2;
  }();
  var InputCheckbox = /* @__PURE__ */ function() {
    function InputCheckbox2() {
    }
    ;
    InputCheckbox2.value = new InputCheckbox2();
    return InputCheckbox2;
  }();
  var InputColor = /* @__PURE__ */ function() {
    function InputColor2() {
    }
    ;
    InputColor2.value = new InputColor2();
    return InputColor2;
  }();
  var InputDate = /* @__PURE__ */ function() {
    function InputDate2() {
    }
    ;
    InputDate2.value = new InputDate2();
    return InputDate2;
  }();
  var InputDatetimeLocal = /* @__PURE__ */ function() {
    function InputDatetimeLocal2() {
    }
    ;
    InputDatetimeLocal2.value = new InputDatetimeLocal2();
    return InputDatetimeLocal2;
  }();
  var InputEmail = /* @__PURE__ */ function() {
    function InputEmail2() {
    }
    ;
    InputEmail2.value = new InputEmail2();
    return InputEmail2;
  }();
  var InputFile = /* @__PURE__ */ function() {
    function InputFile2() {
    }
    ;
    InputFile2.value = new InputFile2();
    return InputFile2;
  }();
  var InputHidden = /* @__PURE__ */ function() {
    function InputHidden2() {
    }
    ;
    InputHidden2.value = new InputHidden2();
    return InputHidden2;
  }();
  var InputImage = /* @__PURE__ */ function() {
    function InputImage2() {
    }
    ;
    InputImage2.value = new InputImage2();
    return InputImage2;
  }();
  var InputMonth = /* @__PURE__ */ function() {
    function InputMonth2() {
    }
    ;
    InputMonth2.value = new InputMonth2();
    return InputMonth2;
  }();
  var InputNumber = /* @__PURE__ */ function() {
    function InputNumber2() {
    }
    ;
    InputNumber2.value = new InputNumber2();
    return InputNumber2;
  }();
  var InputPassword = /* @__PURE__ */ function() {
    function InputPassword2() {
    }
    ;
    InputPassword2.value = new InputPassword2();
    return InputPassword2;
  }();
  var InputRadio = /* @__PURE__ */ function() {
    function InputRadio2() {
    }
    ;
    InputRadio2.value = new InputRadio2();
    return InputRadio2;
  }();
  var InputRange = /* @__PURE__ */ function() {
    function InputRange2() {
    }
    ;
    InputRange2.value = new InputRange2();
    return InputRange2;
  }();
  var InputReset = /* @__PURE__ */ function() {
    function InputReset2() {
    }
    ;
    InputReset2.value = new InputReset2();
    return InputReset2;
  }();
  var InputSearch = /* @__PURE__ */ function() {
    function InputSearch2() {
    }
    ;
    InputSearch2.value = new InputSearch2();
    return InputSearch2;
  }();
  var InputSubmit = /* @__PURE__ */ function() {
    function InputSubmit2() {
    }
    ;
    InputSubmit2.value = new InputSubmit2();
    return InputSubmit2;
  }();
  var InputTel = /* @__PURE__ */ function() {
    function InputTel2() {
    }
    ;
    InputTel2.value = new InputTel2();
    return InputTel2;
  }();
  var InputText = /* @__PURE__ */ function() {
    function InputText2() {
    }
    ;
    InputText2.value = new InputText2();
    return InputText2;
  }();
  var InputTime = /* @__PURE__ */ function() {
    function InputTime2() {
    }
    ;
    InputTime2.value = new InputTime2();
    return InputTime2;
  }();
  var InputUrl = /* @__PURE__ */ function() {
    function InputUrl2() {
    }
    ;
    InputUrl2.value = new InputUrl2();
    return InputUrl2;
  }();
  var InputWeek = /* @__PURE__ */ function() {
    function InputWeek2() {
    }
    ;
    InputWeek2.value = new InputWeek2();
    return InputWeek2;
  }();
  var renderInputType = function(v) {
    if (v instanceof InputButton) {
      return "button";
    }
    ;
    if (v instanceof InputCheckbox) {
      return "checkbox";
    }
    ;
    if (v instanceof InputColor) {
      return "color";
    }
    ;
    if (v instanceof InputDate) {
      return "date";
    }
    ;
    if (v instanceof InputDatetimeLocal) {
      return "datetime-local";
    }
    ;
    if (v instanceof InputEmail) {
      return "email";
    }
    ;
    if (v instanceof InputFile) {
      return "file";
    }
    ;
    if (v instanceof InputHidden) {
      return "hidden";
    }
    ;
    if (v instanceof InputImage) {
      return "image";
    }
    ;
    if (v instanceof InputMonth) {
      return "month";
    }
    ;
    if (v instanceof InputNumber) {
      return "number";
    }
    ;
    if (v instanceof InputPassword) {
      return "password";
    }
    ;
    if (v instanceof InputRadio) {
      return "radio";
    }
    ;
    if (v instanceof InputRange) {
      return "range";
    }
    ;
    if (v instanceof InputReset) {
      return "reset";
    }
    ;
    if (v instanceof InputSearch) {
      return "search";
    }
    ;
    if (v instanceof InputSubmit) {
      return "submit";
    }
    ;
    if (v instanceof InputTel) {
      return "tel";
    }
    ;
    if (v instanceof InputText) {
      return "text";
    }
    ;
    if (v instanceof InputTime) {
      return "time";
    }
    ;
    if (v instanceof InputUrl) {
      return "url";
    }
    ;
    if (v instanceof InputWeek) {
      return "week";
    }
    ;
    throw new Error("Failed pattern match at DOM.HTML.Indexed.InputType (line 33, column 19 - line 55, column 22): " + [v.constructor.name]);
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust2) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value13 = b2;
              while (true) {
                var maybe2 = f(value13);
                if (isNothing2(maybe2))
                  return result;
                var tuple = fromJust2(maybe2);
                result.push(fst2(tuple));
                value13 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust2) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value13 = b2;
              while (true) {
                var tuple = f(value13);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2))
                  return result;
                value13 = fromJust2(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/index.js
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(/* @__PURE__ */ fromJust())(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(/* @__PURE__ */ fromJust())(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };
  var none = function(dictUnfoldable) {
    return unfoldr(dictUnfoldable)($$const(Nothing.value))(unit);
  };

  // output/Data.Enum/index.js
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var toEnumWithDefaults = function(dictBoundedEnum) {
    return function(low2) {
      return function(high2) {
        return function(x) {
          var v = toEnum(dictBoundedEnum)(x);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $54 = x < fromEnum(dictBoundedEnum)(bottom(dictBoundedEnum.Bounded0()));
            if ($54) {
              return low2;
            }
            ;
            return high2;
          }
          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= bottom(boundedInt) && v <= top(boundedInt)) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ function() {
    return {
      cardinality: toCharCode(top(boundedChar)) - toCharCode(bottom(boundedChar)) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  }();

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();
  var singleton2 = function(dictPlus) {
    return function(a2) {
      return new NonEmpty(a2, empty(dictPlus));
    };
  };

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  }();
  var NonEmptyList = function(x) {
    return x;
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_chunksAcc) {
      return function($copy_v) {
        var $tco_var_chunksAcc = $copy_chunksAcc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(chunksAcc, v) {
          if (v instanceof Cons && (v.value1 instanceof Cons && v.value1.value1 instanceof Cons)) {
            $tco_var_chunksAcc = new Cons(v, chunksAcc);
            $copy_v = v.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v1) {
            if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil)) {
              return new Cons(f(v1.value0), new Cons(f(v1.value1.value0), Nil.value));
            }
            ;
            if (v1 instanceof Cons && v1.value1 instanceof Nil) {
              return new Cons(f(v1.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v1) {
            return function($copy_acc) {
              var $tco_var_v1 = $copy_v1;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v1, acc) {
                if (v1 instanceof Cons && (v1.value0 instanceof Cons && (v1.value0.value1 instanceof Cons && v1.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v1 = v1.value1;
                  $copy_acc = new Cons(f(v1.value0.value0), new Cons(f(v1.value0.value1.value0), new Cons(f(v1.value0.value1.value1.value0), acc)));
                  return;
                }
                ;
                $tco_done1 = true;
                return acc;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v1, $copy_acc);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(chunksAcc)(unrolledMap(v));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_chunksAcc, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var foldableList = {
    foldr: function(f) {
      return function(b2) {
        var rev3 = function() {
          var go2 = function($copy_acc) {
            return function($copy_v) {
              var $tco_var_acc = $copy_acc;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(acc, v) {
                if (v instanceof Nil) {
                  $tco_done = true;
                  return acc;
                }
                ;
                if (v instanceof Cons) {
                  $tco_var_acc = new Cons(v.value0, acc);
                  $copy_v = v.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [acc.constructor.name, v.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_acc, $copy_v);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $205 = foldl(foldableList)(flip(f))(b2);
        return function($206) {
          return $205(rev3($206));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b2, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b2;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b2)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $207 = append(dictMonoid.Semigroup0())(acc);
          return function($208) {
            return $207(f($208));
          };
        })(mempty(dictMonoid));
      };
    }
  };
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr(foldableList)(Cons.create)(ys)(xs);
      };
    }
  };
  var altList = {
    alt: /* @__PURE__ */ append(semigroupList),
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();

  // output/Data.Variant/index.js
  var traverseSome = function() {
    return function() {
      return function() {
        return function() {
          return function(dictFunctor) {
            return function(r) {
              return function(k) {
                return function(v) {
                  if (unsafeHas(v.type)(r)) {
                    return mapFlipped(dictFunctor)(unsafeGet(v.type)(r)(v.value))(function(value13) {
                      return {
                        type: v.type,
                        value: value13
                      };
                    });
                  }
                  ;
                  return k(v);
                };
              };
            };
          };
        };
      };
    };
  };
  var traverse2 = function() {
    return function() {
      return function() {
        return function() {
          return function(dictApplicative) {
            return function(r) {
              return traverseSome()()()()(dictApplicative.Apply0().Functor0())(r)(function() {
                var $79 = pure(dictApplicative);
                return function($80) {
                  return $79($80);
                };
              }());
            };
          };
        };
      };
    };
  };
  var overSome = function() {
    return function() {
      return function() {
        return function() {
          return function(r) {
            return function(k) {
              return function(v) {
                if (unsafeHas(v.type)(r)) {
                  return {
                    type: v.type,
                    value: unsafeGet(v.type)(r)(v.value)
                  };
                }
                ;
                return k(v);
              };
            };
          };
        };
      };
    };
  };
  var over2 = function() {
    return function() {
      return function() {
        return function() {
          return function(r) {
            return overSome()()()()(r)(unsafeCoerce2);
          };
        };
      };
    };
  };
  var inj = function() {
    return function(dictIsSymbol) {
      return function(p2) {
        return function(value13) {
          return {
            type: reflectSymbol(dictIsSymbol)(p2),
            value: value13
          };
        };
      };
    };
  };

  // output/Foreign.Object/foreign.js
  function _copyST(m) {
    return function() {
      var r = {};
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r[k] = m[k];
        }
      }
      return r;
    };
  }
  var empty2 = {};
  function runST(f) {
    return f();
  }
  function _fmapObject(m0, f) {
    var m = {};
    for (var k in m0) {
      if (hasOwnProperty.call(m0, k)) {
        m[k] = f(m0[k]);
      }
    }
    return m;
  }
  function _foldM(bind2) {
    return function(f) {
      return function(mz) {
        return function(m) {
          var acc = mz;
          function g(k2) {
            return function(z) {
              return f(z)(k2)(m[k2]);
            };
          }
          for (var k in m) {
            if (hasOwnProperty.call(m, k)) {
              acc = bind2(acc)(g(k));
            }
          }
          return acc;
        };
      };
    };
  }
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a2) {
      return function() {
        return f(a2());
      };
    };
  };
  var pure_ = function(a2) {
    return function() {
      return a2;
    };
  };
  var bind_ = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };
  var foreach = function(as) {
    return function(f) {
      return function() {
        for (var i2 = 0, l = as.length; i2 < l; i2++) {
          f(as[i2])();
        }
      };
    };
  };

  // output/Control.Monad.ST.Internal/index.js
  var $runtime_lazy3 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var functorST = {
    map: map_
  };
  var monadST = {
    Applicative0: function() {
      return applicativeST;
    },
    Bind1: function() {
      return bindST;
    }
  };
  var bindST = {
    bind: bind_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var applicativeST = {
    pure: pure_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var $lazy_applyST = /* @__PURE__ */ $runtime_lazy3("applyST", "Control.Monad.ST.Internal", function() {
    return {
      apply: ap(monadST),
      Functor0: function() {
        return functorST;
      }
    };
  });

  // output/Data.Array/foreign.js
  var replicateFill = function(count) {
    return function(value13) {
      if (count < 1) {
        return [];
      }
      var result = new Array(count);
      return result.fill(value13);
    };
  };
  var replicatePolyfill = function(count) {
    return function(value13) {
      var result = [];
      var n = 0;
      for (var i2 = 0; i2 < count; i2++) {
        result[n++] = value13;
      }
      return result;
    };
  };
  var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons3(head4, tail2) {
      this.head = head4;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head4) {
      return function(tail2) {
        return new Cons3(head4, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr3) {
      return function(xs) {
        return listToArray(foldr3(curryCons)(emptyList)(xs));
      };
    };
  }();
  var length = function(xs) {
    return xs.length;
  };
  var unconsImpl = function(empty7) {
    return function(next) {
      return function(xs) {
        return xs.length === 0 ? empty7({}) : next(xs[0])(xs.slice(1));
      };
    };
  };
  var findIndexImpl = function(just) {
    return function(nothing) {
      return function(f) {
        return function(xs) {
          for (var i2 = 0, l = xs.length; i2 < l; i2++) {
            if (f(xs[i2]))
              return just(i2);
          }
          return nothing;
        };
      };
    };
  };
  var _deleteAt = function(just) {
    return function(nothing) {
      return function(i2) {
        return function(l) {
          if (i2 < 0 || i2 >= l.length)
            return nothing;
          var l1 = l.slice();
          l1.splice(i2, 1);
          return just(l1);
        };
      };
    };
  };
  var sortByImpl = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to) {
      var mid;
      var i2;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from2 + (to - from2 >> 1);
      if (mid - from2 > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
      if (to - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
      i2 = from2;
      j = mid;
      k = from2;
      while (i2 < mid && j < to) {
        x = xs2[i2];
        y = xs2[j];
        c = fromOrdering(compare2(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i2;
        }
      }
      while (i2 < mid) {
        xs1[k++] = xs2[i2++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2) {
      return function(fromOrdering) {
        return function(xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  }();
  var unsafeIndexImpl = function(xs) {
    return function(n) {
      return xs[n];
    };
  };

  // output/Data.Array.ST/foreign.js
  var sortByImpl2 = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to) {
      var mid;
      var i2;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from2 + (to - from2 >> 1);
      if (mid - from2 > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
      if (to - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
      i2 = from2;
      j = mid;
      k = from2;
      while (i2 < mid && j < to) {
        x = xs2[i2];
        y = xs2[j];
        c = fromOrdering(compare2(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i2;
        }
      }
      while (i2 < mid) {
        xs1[k++] = xs2[i2++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2) {
      return function(fromOrdering) {
        return function(xs) {
          return function() {
            if (xs.length < 2)
              return xs;
            mergeFromTo(compare2, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
          };
        };
      };
    };
  }();

  // output/Data.Array/index.js
  var unsafeIndex = function() {
    return unsafeIndexImpl;
  };
  var uncons = /* @__PURE__ */ function() {
    return unconsImpl($$const(Nothing.value))(function(x) {
      return function(xs) {
        return new Just({
          head: x,
          tail: xs
        });
      };
    });
  }();
  var toUnfoldable = function(dictUnfoldable) {
    return function(xs) {
      var len = length(xs);
      var f = function(i2) {
        if (i2 < len) {
          return new Just(new Tuple(unsafeIndex()(xs)(i2), i2 + 1 | 0));
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Array (line 156, column 3 - line 158, column 26): " + [i2.constructor.name]);
      };
      return unfoldr(dictUnfoldable)(f)(0);
    };
  };
  var sortBy = function(comp) {
    return sortByImpl(comp)(function(v) {
      if (v instanceof GT) {
        return 1;
      }
      ;
      if (v instanceof EQ) {
        return 0;
      }
      ;
      if (v instanceof LT) {
        return -1 | 0;
      }
      ;
      throw new Error("Failed pattern match at Data.Array (line 829, column 31 - line 832, column 11): " + [v.constructor.name]);
    });
  };
  var sortWith = function(dictOrd) {
    return function(f) {
      return sortBy(comparing(dictOrd)(f));
    };
  };
  var fromFoldable = function(dictFoldable) {
    return fromFoldableImpl(foldr(dictFoldable));
  };
  var findIndex = /* @__PURE__ */ function() {
    return findIndexImpl(Just.create)(Nothing.value);
  }();
  var elemIndex = function(dictEq) {
    return function(x) {
      return findIndex(function(v) {
        return eq(dictEq)(v)(x);
      });
    };
  };
  var elem2 = function(dictEq) {
    return function(a2) {
      return function(arr) {
        return isJust(elemIndex(dictEq)(a2)(arr));
      };
    };
  };
  var deleteAt = /* @__PURE__ */ function() {
    return _deleteAt(Just.create)(Nothing.value);
  }();
  var deleteBy = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2.length === 0) {
          return [];
        }
        ;
        return maybe(v2)(function(i2) {
          return fromJust()(deleteAt(i2)(v2));
        })(findIndex(v(v1))(v2));
      };
    };
  };

  // output/Foreign.Object.ST/foreign.js
  var newImpl = function() {
    return {};
  };
  function poke2(k) {
    return function(v) {
      return function(m) {
        return function() {
          m[k] = v;
          return m;
        };
      };
    };
  }

  // output/Foreign.Object/index.js
  var toUnfoldable2 = function(dictUnfoldable) {
    var $35 = toUnfoldable(dictUnfoldable);
    var $36 = toArrayWithKey(Tuple.create);
    return function($37) {
      return $35($36($37));
    };
  };
  var thawST = _copyST;
  var singleton3 = function(k) {
    return function(v) {
      return runST(bindFlipped(bindST)(poke2(k)(v))(newImpl));
    };
  };
  var mutate = function(f) {
    return function(m) {
      return runST(function __do2() {
        var s = thawST(m)();
        f(s)();
        return s;
      });
    };
  };
  var lookup2 = /* @__PURE__ */ function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  }();
  var insert3 = function(k) {
    return function(v) {
      return mutate(poke2(k)(v));
    };
  };
  var functorObject = {
    map: function(f) {
      return function(m) {
        return _fmapObject(m, f);
      };
    }
  };
  var fromFoldable2 = function(dictFoldable) {
    return function(l) {
      return runST(function __do2() {
        var s = newImpl();
        foreach(fromFoldable(dictFoldable)(l))(function(v) {
          return $$void(functorST)(poke2(v.value0)(v.value1)(s));
        })();
        return s;
      });
    };
  };
  var foldM = function(dictMonad) {
    return function(f) {
      return function(z) {
        return _foldM(bind(dictMonad.Bind1()))(f)(pure(dictMonad.Applicative0())(z));
      };
    };
  };
  var unionWith = function(f) {
    return function(m1) {
      return function(m2) {
        return mutate(function(s1) {
          return foldM(monadST)(function(s2) {
            return function(k) {
              return function(v1) {
                return poke2(k)(_lookup(v1, function(v2) {
                  return f(v1)(v2);
                }, k, m2))(s2);
              };
            };
          })(s1)(m1);
        })(m2);
      };
    };
  };
  var semigroupObject = function(dictSemigroup) {
    return {
      append: unionWith(append(dictSemigroup))
    };
  };
  var monoidObject = function(dictSemigroup) {
    return {
      mempty: empty2,
      Semigroup0: function() {
        return semigroupObject(dictSemigroup);
      }
    };
  };
  var fold2 = /* @__PURE__ */ _foldM(applyFlipped);

  // output/Foreign.Object.Unsafe/foreign.js
  function unsafeIndex2(m) {
    return function(k) {
      return m[k];
    };
  }

  // output/Data.Exists/index.js
  var runExists = unsafeCoerce2;
  var mkExists = unsafeCoerce2;

  // output/Data.Coyoneda/index.js
  var CoyonedaF = /* @__PURE__ */ function() {
    function CoyonedaF2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CoyonedaF2.create = function(value0) {
      return function(value1) {
        return new CoyonedaF2(value0, value1);
      };
    };
    return CoyonedaF2;
  }();
  var unCoyoneda = function(f) {
    return function(v) {
      return runExists(function(v1) {
        return f(v1.value0)(v1.value1);
      })(v);
    };
  };
  var coyoneda = function(k) {
    return function(fi) {
      return mkExists(new CoyonedaF(k, fi));
    };
  };
  var functorCoyoneda = {
    map: function(f) {
      return function(v) {
        return runExists(function(v1) {
          return coyoneda(function($83) {
            return f(v1.value0($83));
          })(v1.value1);
        })(v);
      };
    }
  };
  var liftCoyoneda = /* @__PURE__ */ coyoneda(/* @__PURE__ */ identity(categoryFn));

  // output/Data.List/index.js
  var reverse2 = /* @__PURE__ */ function() {
    var go2 = function($copy_acc) {
      return function($copy_v) {
        var $tco_var_acc = $copy_acc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(acc, v) {
          if (v instanceof Nil) {
            $tco_done = true;
            return acc;
          }
          ;
          if (v instanceof Cons) {
            $tco_var_acc = new Cons(v.value0, acc);
            $copy_v = v.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [acc.constructor.name, v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_acc, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go2(Nil.value);
  }();
  var $$null = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };

  // output/Data.Map.Internal/index.js
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Two = /* @__PURE__ */ function() {
    function Two3(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Two3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Two3(value0, value1, value22, value32);
          };
        };
      };
    };
    return Two3;
  }();
  var Three = /* @__PURE__ */ function() {
    function Three3(value0, value1, value22, value32, value42, value52, value62) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
      this.value6 = value62;
    }
    ;
    Three3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return function(value62) {
                  return new Three3(value0, value1, value22, value32, value42, value52, value62);
                };
              };
            };
          };
        };
      };
    };
    return Three3;
  }();
  var TwoLeft = /* @__PURE__ */ function() {
    function TwoLeft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoLeft2(value0, value1, value22);
        };
      };
    };
    return TwoLeft2;
  }();
  var TwoRight = /* @__PURE__ */ function() {
    function TwoRight2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoRight2(value0, value1, value22);
        };
      };
    };
    return TwoRight2;
  }();
  var ThreeLeft = /* @__PURE__ */ function() {
    function ThreeLeft2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeLeft2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeLeft2;
  }();
  var ThreeMiddle = /* @__PURE__ */ function() {
    function ThreeMiddle2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeMiddle2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeMiddle2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeMiddle2;
  }();
  var ThreeRight = /* @__PURE__ */ function() {
    function ThreeRight2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeRight2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeRight2;
  }();
  var KickUp = /* @__PURE__ */ function() {
    function KickUp2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    KickUp2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new KickUp2(value0, value1, value22, value32);
          };
        };
      };
    };
    return KickUp2;
  }();
  var lookup3 = function(dictOrd) {
    return function(k) {
      var comp = compare(dictOrd);
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Two) {
            var v2 = comp(k)(v.value1);
            if (v2 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            if (v2 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          if (v instanceof Three) {
            var v3 = comp(k)(v.value1);
            if (v3 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            var v4 = comp(k)(v.value4);
            if (v4 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value5);
            }
            ;
            if (v3 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            if (v4 instanceof GT) {
              $copy_v = v.value6;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var fromZipper = function($copy_dictOrd) {
    return function($copy_v) {
      return function($copy_tree) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictOrd, v, tree) {
          if (v instanceof Nil) {
            $tco_done = true;
            return tree;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(tree, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }
            ;
            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(v.value0.value0, v.value0.value1, v.value0.value2, tree);
              return;
            }
            ;
            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(tree, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, tree, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, tree);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, tree.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_tree);
        }
        ;
        return $tco_result;
      };
    };
  };
  var insert4 = function(dictOrd) {
    return function(k) {
      return function(v) {
        var up = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
              }
              ;
              if (v1 instanceof Cons) {
                if (v1.value0 instanceof TwoLeft) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }
                ;
                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper(dictOrd)(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                }
                ;
                if (v1.value0 instanceof ThreeLeft) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeMiddle) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeRight) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        var comp = compare(dictOrd);
        var down = function($copy_ctx) {
          return function($copy_v1) {
            var $tco_var_ctx = $copy_ctx;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(ctx, v1) {
              if (v1 instanceof Leaf) {
                $tco_done1 = true;
                return up(ctx)(new KickUp(Leaf.value, k, v, Leaf.value));
              }
              ;
              if (v1 instanceof Two) {
                var v2 = comp(k)(v1.value1);
                if (v2 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper(dictOrd)(ctx)(new Two(v1.value0, k, v, v1.value3));
                }
                ;
                if (v2 instanceof LT) {
                  $tco_var_ctx = new Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }
                ;
                $tco_var_ctx = new Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
                $copy_v1 = v1.value3;
                return;
              }
              ;
              if (v1 instanceof Three) {
                var v3 = comp(k)(v1.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper(dictOrd)(ctx)(new Three(v1.value0, k, v, v1.value3, v1.value4, v1.value5, v1.value6));
                }
                ;
                var v4 = comp(k)(v1.value4);
                if (v4 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper(dictOrd)(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k, v, v1.value6));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_ctx = new Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }
                ;
                if (v3 instanceof GT && v4 instanceof LT) {
                  $tco_var_ctx = new Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value3;
                  return;
                }
                ;
                $tco_var_ctx = new Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                $copy_v1 = v1.value6;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [ctx.constructor.name, v1.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_ctx, $copy_v1);
            }
            ;
            return $tco_result;
          };
        };
        return down(Nil.value);
      };
    };
  };
  var pop = function(dictOrd) {
    return function(k) {
      var up = function($copy_ctxs) {
        return function($copy_tree) {
          var $tco_var_ctxs = $copy_ctxs;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(ctxs, tree) {
            if (ctxs instanceof Nil) {
              $tco_done = true;
              return tree;
            }
            ;
            if (ctxs instanceof Cons) {
              if (ctxs.value0 instanceof TwoLeft && (ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                $tco_done = true;
                return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              $tco_done = true;
              return unsafeCrashWith("The impossible happened in partial function `up`.");
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): " + [ctxs.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctxs, $copy_tree);
          }
          ;
          return $tco_result;
        };
      };
      var removeMaxNode = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Two && (m.value0 instanceof Leaf && m.value3 instanceof Leaf)) {
              $tco_done1 = true;
              return up(ctx)(Leaf.value);
            }
            ;
            if (m instanceof Two) {
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three && (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf))) {
              $tco_done1 = true;
              return up(new Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
            }
            ;
            if (m instanceof Three) {
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            $tco_done1 = true;
            return unsafeCrashWith("The impossible happened in partial function `removeMaxNode`.");
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      var maxNode = function($copy_m) {
        var $tco_done2 = false;
        var $tco_result;
        function $tco_loop(m) {
          if (m instanceof Two && m.value3 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value1,
              value: m.value2
            };
          }
          ;
          if (m instanceof Two) {
            $copy_m = m.value3;
            return;
          }
          ;
          if (m instanceof Three && m.value6 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value4,
              value: m.value5
            };
          }
          ;
          if (m instanceof Three) {
            $copy_m = m.value6;
            return;
          }
          ;
          $tco_done2 = true;
          return unsafeCrashWith("The impossible happened in partial function `maxNode`.");
        }
        ;
        while (!$tco_done2) {
          $tco_result = $tco_loop($copy_m);
        }
        ;
        return $tco_result;
      };
      var comp = compare(dictOrd);
      var down = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done3 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Leaf) {
              $tco_done3 = true;
              return Nothing.value;
            }
            ;
            if (m instanceof Two) {
              var v = comp(k)(m.value1);
              if (m.value3 instanceof Leaf && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, up(ctx)(Leaf.value)));
              }
              ;
              if (v instanceof EQ) {
                var max6 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new TwoLeft(max6.key, max6.value, m.value3), ctx))(m.value0)));
              }
              ;
              if (v instanceof LT) {
                $tco_var_ctx = new Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three) {
              var leaves = function() {
                if (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf)) {
                  return true;
                }
                ;
                return false;
              }();
              var v = comp(k)(m.value4);
              var v3 = comp(k)(m.value1);
              if (leaves && v3 instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
              }
              ;
              if (leaves && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
              }
              ;
              if (v3 instanceof EQ) {
                var max6 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new ThreeLeft(max6.key, max6.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
              }
              ;
              if (v instanceof EQ) {
                var max6 = maxNode(m.value3);
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, removeMaxNode(new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max6.key, max6.value, m.value6), ctx))(m.value3)));
              }
              ;
              if (v3 instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              if (v3 instanceof GT && v instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value3;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): " + [m.constructor.name]);
          }
          ;
          while (!$tco_done3) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      return down(Nil.value);
    };
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(z)(m.value3)))(m.value0);
          }
          ;
          if (m instanceof Three) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(f(m.value5)(foldr(foldableMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3);
          }
          ;
          if (m instanceof Three) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty(dictMonoid);
          }
          ;
          if (m instanceof Two) {
            return append(dictMonoid.Semigroup0())(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append(dictMonoid.Semigroup0())(f(m.value2))(foldMap(foldableMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append(dictMonoid.Semigroup0())(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append(dictMonoid.Semigroup0())(f(m.value2))(append(dictMonoid.Semigroup0())(foldMap(foldableMap)(dictMonoid)(f)(m.value3))(append(dictMonoid.Semigroup0())(f(m.value5))(foldMap(foldableMap)(dictMonoid)(f)(m.value6)))));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): " + [m.constructor.name]);
        };
      };
    }
  };
  var empty3 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var $$delete2 = function(dictOrd) {
    return function(k) {
      return function(m) {
        return maybe(m)(snd)(pop(dictOrd)(k)(m));
      };
    };
  };
  var alter = function(dictOrd) {
    return function(f) {
      return function(k) {
        return function(m) {
          var v = f(lookup3(dictOrd)(k)(m));
          if (v instanceof Nothing) {
            return $$delete2(dictOrd)(k)(m);
          }
          ;
          if (v instanceof Just) {
            return insert4(dictOrd)(k)(v.value0)(m);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): " + [v.constructor.name]);
        };
      };
    };
  };

  // output/Halogen.Data.OrdBox/index.js
  var OrdBox = /* @__PURE__ */ function() {
    function OrdBox2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    OrdBox2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new OrdBox2(value0, value1, value22);
        };
      };
    };
    return OrdBox2;
  }();
  var mkOrdBox = function(dictOrd) {
    return OrdBox.create(eq(dictOrd.Eq0()))(compare(dictOrd));
  };
  var eqOrdBox = {
    eq: function(v) {
      return function(v1) {
        return v.value0(v.value2)(v1.value2);
      };
    }
  };
  var ordOrdBox = {
    compare: function(v) {
      return function(v1) {
        return v.value1(v.value2)(v1.value2);
      };
    },
    Eq0: function() {
      return eqOrdBox;
    }
  };

  // output/Halogen.Data.Slot/index.js
  var pop2 = function() {
    return function(dictIsSymbol) {
      return function(dictOrd) {
        return function(sym) {
          return function(key) {
            return function(v) {
              return pop(ordTuple(ordString)(ordOrdBox))(new Tuple(reflectSymbol(dictIsSymbol)(sym), mkOrdBox(dictOrd)(key)))(v);
            };
          };
        };
      };
    };
  };
  var lookup4 = function() {
    return function(dictIsSymbol) {
      return function(dictOrd) {
        return function(sym) {
          return function(key) {
            return function(v) {
              return lookup3(ordTuple(ordString)(ordOrdBox))(new Tuple(reflectSymbol(dictIsSymbol)(sym), mkOrdBox(dictOrd)(key)))(v);
            };
          };
        };
      };
    };
  };
  var insert5 = function() {
    return function(dictIsSymbol) {
      return function(dictOrd) {
        return function(sym) {
          return function(key) {
            return function(val) {
              return function(v) {
                return insert4(ordTuple(ordString)(ordOrdBox))(new Tuple(reflectSymbol(dictIsSymbol)(sym), mkOrdBox(dictOrd)(key)))(val)(v);
              };
            };
          };
        };
      };
    };
  };
  var foreachSlot = function(dictApplicative) {
    return function(v) {
      return function(k) {
        return traverse_(dictApplicative)(foldableMap)(function($33) {
          return k($33);
        })(v);
      };
    };
  };
  var empty4 = empty3;

  // output/Data.MediaType/index.js
  var eqMediaType = {
    eq: function(x) {
      return function(y) {
        return x === y;
      };
    }
  };

  // output/Halogen.Query.Input/index.js
  var RefUpdate = /* @__PURE__ */ function() {
    function RefUpdate2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RefUpdate2.create = function(value0) {
      return function(value1) {
        return new RefUpdate2(value0, value1);
      };
    };
    return RefUpdate2;
  }();
  var Action = /* @__PURE__ */ function() {
    function Action3(value0) {
      this.value0 = value0;
    }
    ;
    Action3.create = function(value0) {
      return new Action3(value0);
    };
    return Action3;
  }();

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a2, r, f) {
    return a2 == null ? r : f(a2);
  }
  function notNull(x) {
    return x;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Halogen.VDom.Machine/index.js
  var Step = /* @__PURE__ */ function() {
    function Step3(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Step3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Step3(value0, value1, value22, value32);
          };
        };
      };
    };
    return Step3;
  }();
  var unStep = unsafeCoerce2;
  var step2 = function(v, a2) {
    return v.value2(v.value1, a2);
  };
  var mkStep = unsafeCoerce2;
  var halt = function(v) {
    return v.value3(v.value1);
  };
  var extract2 = /* @__PURE__ */ unStep(function(v) {
    return v.value0;
  });

  // output/Halogen.VDom.Types/index.js
  var Text = /* @__PURE__ */ function() {
    function Text2(value0) {
      this.value0 = value0;
    }
    ;
    Text2.create = function(value0) {
      return new Text2(value0);
    };
    return Text2;
  }();
  var Elem = /* @__PURE__ */ function() {
    function Elem2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Elem2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Elem2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Elem2;
  }();
  var Keyed = /* @__PURE__ */ function() {
    function Keyed2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Keyed2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Keyed2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Keyed2;
  }();
  var Widget = /* @__PURE__ */ function() {
    function Widget2(value0) {
      this.value0 = value0;
    }
    ;
    Widget2.create = function(value0) {
      return new Widget2(value0);
    };
    return Widget2;
  }();
  var Grafted = /* @__PURE__ */ function() {
    function Grafted2(value0) {
      this.value0 = value0;
    }
    ;
    Grafted2.create = function(value0) {
      return new Grafted2(value0);
    };
    return Grafted2;
  }();
  var Graft = /* @__PURE__ */ function() {
    function Graft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Graft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Graft2(value0, value1, value22);
        };
      };
    };
    return Graft2;
  }();
  var unGraft = function(f) {
    return function($55) {
      return f($55);
    };
  };
  var graft = unsafeCoerce2;
  var bifunctorGraft = {
    bimap: function(f) {
      return function(g) {
        return unGraft(function(v) {
          return graft(new Graft(function($57) {
            return f(v.value0($57));
          }, function($58) {
            return g(v.value1($58));
          }, v.value2));
        });
      };
    }
  };
  var runGraft = /* @__PURE__ */ unGraft(function(v) {
    var go2 = function(v2) {
      if (v2 instanceof Text) {
        return new Text(v2.value0);
      }
      ;
      if (v2 instanceof Elem) {
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map(functorArray)(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map(functorArray)(map(functorTuple)(go2))(v2.value3));
      }
      ;
      if (v2 instanceof Widget) {
        return new Widget(v.value1(v2.value0));
      }
      ;
      if (v2 instanceof Grafted) {
        return new Grafted(bimap(bifunctorGraft)(v.value0)(v.value1)(v2.value0));
      }
      ;
      throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [v2.constructor.name]);
    };
    return go2(v.value2);
  });

  // output/Halogen.VDom.Util/foreign.js
  "use strict";
  function unsafeGetAny(key, obj) {
    return obj[key];
  }
  function unsafeHasAny(key, obj) {
    return obj.hasOwnProperty(key);
  }
  function unsafeSetAny(key, val, obj) {
    obj[key] = val;
  }
  function forE2(a2, f) {
    var b2 = [];
    for (var i2 = 0; i2 < a2.length; i2++) {
      b2.push(f(i2, a2[i2]));
    }
    return b2;
  }
  function forEachE(a2, f) {
    for (var i2 = 0; i2 < a2.length; i2++) {
      f(a2[i2]);
    }
  }
  function forInE(o, f) {
    var ks = Object.keys(o);
    for (var i2 = 0; i2 < ks.length; i2++) {
      var k = ks[i2];
      f(k, o[k]);
    }
  }
  function diffWithIxE(a1, a2, f1, f2, f3) {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i2 = 0;
    while (1) {
      if (i2 < l1) {
        if (i2 < l2) {
          a3.push(f1(i2, a1[i2], a2[i2]));
        } else {
          f2(i2, a1[i2]);
        }
      } else if (i2 < l2) {
        a3.push(f3(i2, a2[i2]));
      } else {
        break;
      }
      i2++;
    }
    return a3;
  }
  function strMapWithIxE(as, fk, f) {
    var o = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      o[k] = f(k, i2, a2);
    }
    return o;
  }
  function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
    var o2 = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i2, o1[k], a2);
      } else {
        o2[k] = f3(k, i2, a2);
      }
    }
    for (var k in o1) {
      if (k in o2) {
        continue;
      }
      f2(k, o1[k]);
    }
    return o2;
  }
  function refEq2(a2, b2) {
    return a2 === b2;
  }
  function createTextNode(s, doc) {
    return doc.createTextNode(s);
  }
  function setTextContent(s, n) {
    n.textContent = s;
  }
  function createElement(ns, name17, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name17);
    } else {
      return doc.createElement(name17);
    }
  }
  function insertChildIx(i2, a2, b2) {
    var n = b2.childNodes.item(i2) || null;
    if (n !== a2) {
      b2.insertBefore(a2, n);
    }
  }
  function removeChild(a2, b2) {
    if (b2 && a2.parentNode === b2) {
      b2.removeChild(a2);
    }
  }
  function parentNode(a2) {
    return a2.parentNode;
  }
  function setAttribute(ns, attr3, val, el) {
    if (ns != null) {
      el.setAttributeNS(ns, attr3, val);
    } else {
      el.setAttribute(attr3, val);
    }
  }
  function removeAttribute(ns, attr3, el) {
    if (ns != null) {
      el.removeAttributeNS(ns, attr3);
    } else {
      el.removeAttribute(attr3);
    }
  }
  function hasAttribute(ns, attr3, el) {
    if (ns != null) {
      return el.hasAttributeNS(ns, attr3);
    } else {
      return el.hasAttribute(attr3);
    }
  }
  function addEventListener(ev, listener, el) {
    el.addEventListener(ev, listener, false);
  }
  function removeEventListener(ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  }
  var jsUndefined = void 0;

  // output/Halogen.VDom.Util/index.js
  var unsafeLookup = unsafeGetAny;
  var unsafeFreeze2 = unsafeCoerce2;
  var pokeMutMap = unsafeSetAny;
  var newMutMap = newImpl;

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name17) {
    return function(doctype) {
      return doctype[name17];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp = function(name17) {
    return function(node) {
      return function() {
        return node[name17];
      };
    };
  };
  var children = getEffProp("children");
  var _firstElementChild = getEffProp("firstElementChild");
  var _lastElementChild = getEffProp("lastElementChild");
  var childElementCount = getEffProp("childElementCount");
  function _querySelector(selector) {
    return function(node) {
      return function() {
        return node.querySelector(selector);
      };
    };
  }

  // output/Web.DOM.ParentNode/index.js
  var querySelector = function(qs) {
    var $0 = map(functorEffect)(toMaybe);
    var $1 = _querySelector(qs);
    return function($2) {
      return $0($1($2));
    };
  };

  // output/Web.Internal.FFI/foreign.js
  function _unsafeReadProtoTagged(nothing, just, name17, value13) {
    if (typeof window !== "undefined") {
      var ty = window[name17];
      if (ty != null && value13 instanceof ty) {
        return just(value13);
      }
    }
    var obj = value13;
    while (obj != null) {
      var proto = Object.getPrototypeOf(obj);
      var constructorName = proto.constructor.name;
      if (constructorName === name17) {
        return just(value13);
      } else if (constructorName === "Object") {
        return nothing;
      }
      obj = proto;
    }
    return nothing;
  }

  // output/Web.Internal.FFI/index.js
  var unsafeReadProtoTagged = function(name17) {
    return function(value13) {
      return _unsafeReadProtoTagged(Nothing.value, Just.create, name17, value13);
    };
  };

  // output/Web.DOM.Element/index.js
  var toNode = unsafeCoerce2;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy4 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var haltWidget = function(v) {
    return halt(v.widget);
  };
  var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy4("patchWidget", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Widget) {
        var res = step2(state3.widget, vdom.value0);
        var res$prime = unStep(function(v) {
          return mkStep(new Step(v.value0, {
            build: state3.build,
            widget: res
          }, $lazy_patchWidget(296), haltWidget));
        })(res);
        return res$prime;
      }
      ;
      haltWidget(state3);
      return state3.build(vdom);
    };
  });
  var patchWidget = /* @__PURE__ */ $lazy_patchWidget(286);
  var haltText = function(v) {
    var parent2 = parentNode(v.node);
    return removeChild(v.node, parent2);
  };
  var $lazy_patchText = /* @__PURE__ */ $runtime_lazy4("patchText", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchText(82)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Text) {
        if (state3.value === vdom.value0) {
          return mkStep(new Step(state3.node, state3, $lazy_patchText(85), haltText));
        }
        ;
        if (otherwise) {
          var nextState = {
            build: state3.build,
            node: state3.node,
            value: vdom.value0
          };
          setTextContent(vdom.value0, state3.node);
          return mkStep(new Step(state3.node, nextState, $lazy_patchText(89), haltText));
        }
        ;
      }
      ;
      haltText(state3);
      return state3.build(vdom);
    };
  });
  var patchText = /* @__PURE__ */ $lazy_patchText(77);
  var haltKeyed = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forInE(v.children, function(v1, s) {
      return halt(s);
    });
    return halt(v.attrs);
  };
  var haltElem = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forEachE(v.children, halt);
    return halt(v.attrs);
  };
  var eqElemSpec = function(ns1, v, ns2, v1) {
    var $58 = v === v1;
    if ($58) {
      if (ns1 instanceof Just && (ns2 instanceof Just && ns1.value0 === ns2.value0)) {
        return true;
      }
      ;
      if (ns1 instanceof Nothing && ns2 instanceof Nothing) {
        return true;
      }
      ;
      return false;
    }
    ;
    return false;
  };
  var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy4("patchElem", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        var v1 = length(state3.children);
        if (v1 === 0 && v === 0) {
          var attrs2 = step2(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchElem(149), haltElem));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(ix, s, v2) {
          var res = step2(s, v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var onThat = function(ix, v2) {
          var res = state3.build(v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
        var attrs2 = step2(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchElem(172), haltElem));
      }
      ;
      haltElem(state3);
      return state3.build(vdom);
    };
  });
  var patchElem = /* @__PURE__ */ $lazy_patchElem(130);
  var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy4("patchKeyed", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        if (state3.length === 0 && v === 0) {
          var attrs2 = step2(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children,
            length: 0
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(237), haltKeyed));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(v2, ix$prime, s, v3) {
          var res = step2(s, v3.value1);
          insertChildIx(ix$prime, extract2(res), state3.node);
          return res;
        };
        var onThat = function(v2, ix, v3) {
          var res = state3.build(v3.value1);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
        var attrs2 = step2(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2,
          length: v
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(261), haltKeyed));
      }
      ;
      haltKeyed(state3);
      return state3.build(vdom);
    };
  });
  var patchKeyed = /* @__PURE__ */ $lazy_patchKeyed(217);
  var buildWidget = function(v, build2, w) {
    var res = v.buildWidget(v)(w);
    var res$prime = unStep(function(v1) {
      return mkStep(new Step(v1.value0, {
        build: build2,
        widget: res
      }, patchWidget, haltWidget));
    })(res);
    return res$prime;
  };
  var buildText = function(v, build2, s) {
    var node = createTextNode(s, v.document);
    var state3 = {
      build: build2,
      node,
      value: s
    };
    return mkStep(new Step(node, state3, patchText, haltText));
  };
  var buildKeyed = function(v, build2, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode(el);
    var onChild = function(v1, ix, v2) {
      var res = build2(v2.value1);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = strMapWithIxE(ch1, fst, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build: build2,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2,
      length: length(ch1)
    };
    return mkStep(new Step(node, state3, patchKeyed, haltKeyed));
  };
  var buildElem = function(v, build2, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode(el);
    var onChild = function(ix, child) {
      var res = build2(child);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = forE2(ch1, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build: build2,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2
    };
    return mkStep(new Step(node, state3, patchElem, haltElem));
  };
  var buildVDom = function(spec) {
    var $lazy_build = $runtime_lazy4("build", "Halogen.VDom.DOM", function() {
      return function(v) {
        if (v instanceof Text) {
          return buildText(spec, $lazy_build(59), v.value0);
        }
        ;
        if (v instanceof Elem) {
          return buildElem(spec, $lazy_build(60), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Keyed) {
          return buildKeyed(spec, $lazy_build(61), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Widget) {
          return buildWidget(spec, $lazy_build(62), v.value0);
        }
        ;
        if (v instanceof Grafted) {
          return $lazy_build(63)(runGraft(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [v.constructor.name]);
      };
    });
    var build2 = $lazy_build(58);
    return build2;
  };

  // output/Foreign/foreign.js
  function typeOf(value13) {
    return typeof value13;
  }
  function tagOf(value13) {
    return Object.prototype.toString.call(value13).slice(8, -1);
  }
  var isArray = Array.isArray || function(value13) {
    return Object.prototype.toString.call(value13) === "[object Array]";
  };

  // output/Data.List.NonEmpty/index.js
  var singleton5 = /* @__PURE__ */ function() {
    var $169 = singleton2(plusList);
    return function($170) {
      return NonEmptyList($169($170));
    };
  }();
  var cons3 = function(y) {
    return function(v) {
      return new NonEmpty(y, new Cons(v.value0, v.value1));
    };
  };

  // output/Data.String.CodeUnits/foreign.js
  var singleton6 = function(c) {
    return c;
  };
  var length3 = function(s) {
    return s.length;
  };
  var _indexOf = function(just) {
    return function(nothing) {
      return function(x) {
        return function(s) {
          var i2 = s.indexOf(x);
          return i2 === -1 ? nothing : just(i2);
        };
      };
    };
  };
  var take2 = function(n) {
    return function(s) {
      return s.substr(0, n);
    };
  };
  var drop2 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };
  var splitAt = function(i2) {
    return function(s) {
      return { before: s.substring(0, i2), after: s.substring(i2) };
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i2) {
    return function(s) {
      if (i2 >= 0 && i2 < s.length)
        return s.charAt(i2);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Data.String.CodeUnits/index.js
  var stripPrefix = function(v) {
    return function(str) {
      var v1 = splitAt(length3(v))(str);
      var $15 = v1.before === v;
      if ($15) {
        return new Just(v1.after);
      }
      ;
      return Nothing.value;
    };
  };
  var indexOf = /* @__PURE__ */ function() {
    return _indexOf(Just.create)(Nothing.value);
  }();
  var contains = function(pat) {
    var $18 = indexOf(pat);
    return function($19) {
      return isJust($18($19));
    };
  };

  // output/Foreign/index.js
  var TypeMismatch = /* @__PURE__ */ function() {
    function TypeMismatch3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TypeMismatch3.create = function(value0) {
      return function(value1) {
        return new TypeMismatch3(value0, value1);
      };
    };
    return TypeMismatch3;
  }();
  var unsafeToForeign = unsafeCoerce2;
  var unsafeFromForeign = unsafeCoerce2;
  var fail = function(dictMonad) {
    var $115 = throwError(monadThrowExceptT(dictMonad));
    return function($116) {
      return $115(singleton5($116));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    return function(tag) {
      return function(value13) {
        if (tagOf(value13) === tag) {
          return pure(applicativeExceptT(dictMonad))(unsafeFromForeign(value13));
        }
        ;
        if (otherwise) {
          return fail(dictMonad)(new TypeMismatch(tag, tagOf(value13)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value13.constructor.name]);
      };
    };
  };
  var readBoolean = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("Boolean");
  };
  var readString = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("String");
  };

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener2(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener2(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Halogen.VDom.DOM.Prop/index.js
  var $runtime_lazy5 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Created = /* @__PURE__ */ function() {
    function Created2(value0) {
      this.value0 = value0;
    }
    ;
    Created2.create = function(value0) {
      return new Created2(value0);
    };
    return Created2;
  }();
  var Removed = /* @__PURE__ */ function() {
    function Removed2(value0) {
      this.value0 = value0;
    }
    ;
    Removed2.create = function(value0) {
      return new Removed2(value0);
    };
    return Removed2;
  }();
  var Attribute = /* @__PURE__ */ function() {
    function Attribute2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Attribute2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Attribute2(value0, value1, value22);
        };
      };
    };
    return Attribute2;
  }();
  var Property = /* @__PURE__ */ function() {
    function Property2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Property2.create = function(value0) {
      return function(value1) {
        return new Property2(value0, value1);
      };
    };
    return Property2;
  }();
  var Handler = /* @__PURE__ */ function() {
    function Handler2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Handler2.create = function(value0) {
      return function(value1) {
        return new Handler2(value0, value1);
      };
    };
    return Handler2;
  }();
  var Ref = /* @__PURE__ */ function() {
    function Ref2(value0) {
      this.value0 = value0;
    }
    ;
    Ref2.create = function(value0) {
      return new Ref2(value0);
    };
    return Ref2;
  }();
  var unsafeGetProperty = unsafeGetAny;
  var setProperty = unsafeSetAny;
  var removeProperty = function(key, el) {
    var v = hasAttribute(nullImpl, key, el);
    if (v) {
      return removeAttribute(nullImpl, key, el);
    }
    ;
    var v1 = typeOf(unsafeGetAny(key, el));
    if (v1 === "string") {
      return unsafeSetAny(key, "", el);
    }
    ;
    if (key === "rowSpan") {
      return unsafeSetAny(key, 1, el);
    }
    ;
    if (key === "colSpan") {
      return unsafeSetAny(key, 1, el);
    }
    ;
    return unsafeSetAny(key, jsUndefined, el);
  };
  var propToStrKey = function(v) {
    if (v instanceof Attribute && v.value0 instanceof Just) {
      return "attr/" + (v.value0.value0 + (":" + v.value1));
    }
    ;
    if (v instanceof Attribute) {
      return "attr/:" + v.value1;
    }
    ;
    if (v instanceof Property) {
      return "prop/" + v.value0;
    }
    ;
    if (v instanceof Handler) {
      return "handler/" + v.value0;
    }
    ;
    if (v instanceof Ref) {
      return "ref";
    }
    ;
    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [v.constructor.name]);
  };
  var propFromString = unsafeCoerce2;
  var propFromBoolean = unsafeCoerce2;
  var buildProp = function(emit) {
    return function(el) {
      var removeProp = function(prevEvents) {
        return function(v, v1) {
          if (v1 instanceof Attribute) {
            return removeAttribute(toNullable(v1.value0), v1.value1, el);
          }
          ;
          if (v1 instanceof Property) {
            return removeProperty(v1.value0, el);
          }
          ;
          if (v1 instanceof Handler) {
            var handler3 = unsafeLookup(v1.value0, prevEvents);
            return removeEventListener(v1.value0, fst(handler3), el);
          }
          ;
          if (v1 instanceof Ref) {
            return unit;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [v1.constructor.name]);
        };
      };
      var mbEmit = function(v) {
        if (v instanceof Just) {
          return emit(v.value0)();
        }
        ;
        return unit;
      };
      var haltProp = function(state3) {
        var v = lookup2("ref")(state3.props);
        if (v instanceof Just && v.value0 instanceof Ref) {
          return mbEmit(v.value0.value0(new Removed(el)));
        }
        ;
        return unit;
      };
      var diffProp = function(prevEvents, events) {
        return function(v, v1, v11, v2) {
          if (v11 instanceof Attribute && v2 instanceof Attribute) {
            var $57 = v11.value2 === v2.value2;
            if ($57) {
              return v2;
            }
            ;
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v11 instanceof Property && v2 instanceof Property) {
            var v4 = refEq2(v11.value1, v2.value1);
            if (v4) {
              return v2;
            }
            ;
            if (v2.value0 === "value") {
              var elVal = unsafeGetProperty("value", el);
              var $66 = refEq2(elVal, v2.value1);
              if ($66) {
                return v2;
              }
              ;
              setProperty(v2.value0, v2.value1, el);
              return v2;
            }
            ;
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v11 instanceof Handler && v2 instanceof Handler) {
            var handler3 = unsafeLookup(v2.value0, prevEvents);
            write(v2.value1)(snd(handler3))();
            pokeMutMap(v2.value0, handler3, events);
            return v2;
          }
          ;
          return v2;
        };
      };
      var applyProp = function(events) {
        return function(v, v1, v2) {
          if (v2 instanceof Attribute) {
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v2 instanceof Property) {
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v2 instanceof Handler) {
            var v3 = unsafeGetAny(v2.value0, events);
            if (unsafeHasAny(v2.value0, events)) {
              write(v2.value1)(snd(v3))();
              return v2;
            }
            ;
            var ref2 = $$new(v2.value1)();
            var listener = eventListener(function(ev) {
              return function __do2() {
                var f$prime = read(ref2)();
                return mbEmit(f$prime(ev));
              };
            })();
            pokeMutMap(v2.value0, new Tuple(listener, ref2), events);
            addEventListener(v2.value0, listener, el);
            return v2;
          }
          ;
          if (v2 instanceof Ref) {
            mbEmit(v2.value0(new Created(el)));
            return v2;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [v2.constructor.name]);
        };
      };
      var $lazy_patchProp = $runtime_lazy5("patchProp", "Halogen.VDom.DOM.Prop", function() {
        return function(state3, ps2) {
          var events = newMutMap();
          var onThis = removeProp(state3.events);
          var onThese = diffProp(state3.events, events);
          var onThat = applyProp(events);
          var props = diffWithKeyAndIxE(state3.props, ps2, propToStrKey, onThese, onThis, onThat);
          var nextState = {
            events: unsafeFreeze2(events),
            props
          };
          return mkStep(new Step(unit, nextState, $lazy_patchProp(100), haltProp));
        };
      });
      var patchProp = $lazy_patchProp(87);
      var renderProp = function(ps1) {
        var events = newMutMap();
        var ps1$prime = strMapWithIxE(ps1, propToStrKey, applyProp(events));
        var state3 = {
          events: unsafeFreeze2(events),
          props: ps1$prime
        };
        return mkStep(new Step(unit, state3, patchProp, haltProp));
      };
      return renderProp;
    };
  };

  // output/Web.HTML.Common/index.js
  var ClassName = function(x) {
    return x;
  };

  // output/Halogen.HTML.Core/index.js
  var HTML = function(x) {
    return x;
  };
  var widget = function($19) {
    return HTML(Widget.create($19));
  };
  var toPropValue = function(dict) {
    return dict.toPropValue;
  };
  var text = function($20) {
    return HTML(Text.create($20));
  };
  var prop = function(dictIsProp) {
    return function(v) {
      var $22 = Property.create(v);
      var $23 = toPropValue(dictIsProp);
      return function($24) {
        return $22($23($24));
      };
    };
  };
  var isPropString = {
    toPropValue: propFromString
  };
  var isPropInputType = {
    toPropValue: function($37) {
      return propFromString(renderInputType($37));
    }
  };
  var isPropButtonType = {
    toPropValue: function($42) {
      return propFromString(renderButtonType($42));
    }
  };
  var isPropBoolean = {
    toPropValue: propFromBoolean
  };
  var handler = /* @__PURE__ */ function() {
    return Handler.create;
  }();
  var element = function(ns) {
    return function(name17) {
      return function(props) {
        return function(children2) {
          return new Elem(ns, name17, props, children2);
        };
      };
    };
  };
  var attr = function(ns) {
    return function(v) {
      return Attribute.create(ns)(v);
    };
  };

  // output/Control.Applicative.Free/index.js
  var Pure = /* @__PURE__ */ function() {
    function Pure2(value0) {
      this.value0 = value0;
    }
    ;
    Pure2.create = function(value0) {
      return new Pure2(value0);
    };
    return Pure2;
  }();
  var Lift = /* @__PURE__ */ function() {
    function Lift3(value0) {
      this.value0 = value0;
    }
    ;
    Lift3.create = function(value0) {
      return new Lift3(value0);
    };
    return Lift3;
  }();
  var Ap = /* @__PURE__ */ function() {
    function Ap2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Ap2.create = function(value0) {
      return function(value1) {
        return new Ap2(value0, value1);
      };
    };
    return Ap2;
  }();
  var mkAp = function(fba) {
    return function(fb) {
      return new Ap(fba, fb);
    };
  };
  var liftFreeAp = /* @__PURE__ */ function() {
    return Lift.create;
  }();
  var goLeft = function($copy_dictApplicative) {
    return function($copy_fStack) {
      return function($copy_valStack) {
        return function($copy_nat) {
          return function($copy_func) {
            return function($copy_count) {
              var $tco_var_dictApplicative = $copy_dictApplicative;
              var $tco_var_fStack = $copy_fStack;
              var $tco_var_valStack = $copy_valStack;
              var $tco_var_nat = $copy_nat;
              var $tco_var_func = $copy_func;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(dictApplicative, fStack, valStack, nat, func, count) {
                if (func instanceof Pure) {
                  $tco_done = true;
                  return new Tuple(new Cons({
                    func: pure(dictApplicative)(func.value0),
                    count
                  }, fStack), valStack);
                }
                ;
                if (func instanceof Lift) {
                  $tco_done = true;
                  return new Tuple(new Cons({
                    func: nat(func.value0),
                    count
                  }, fStack), valStack);
                }
                ;
                if (func instanceof Ap) {
                  $tco_var_dictApplicative = dictApplicative;
                  $tco_var_fStack = fStack;
                  $tco_var_valStack = cons3(func.value1)(valStack);
                  $tco_var_nat = nat;
                  $tco_var_func = func.value0;
                  $copy_count = count + 1 | 0;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_dictApplicative, $tco_var_fStack, $tco_var_valStack, $tco_var_nat, $tco_var_func, $copy_count);
              }
              ;
              return $tco_result;
            };
          };
        };
      };
    };
  };
  var goApply = function($copy_dictApplicative) {
    return function($copy_fStack) {
      return function($copy_vals) {
        return function($copy_gVal) {
          var $tco_var_dictApplicative = $copy_dictApplicative;
          var $tco_var_fStack = $copy_fStack;
          var $tco_var_vals = $copy_vals;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(dictApplicative, fStack, vals, gVal) {
            if (fStack instanceof Nil) {
              $tco_done = true;
              return new Left(gVal);
            }
            ;
            if (fStack instanceof Cons) {
              var gRes = apply(dictApplicative.Apply0())(fStack.value0.func)(gVal);
              var $14 = fStack.value0.count === 1;
              if ($14) {
                if (fStack.value1 instanceof Nil) {
                  $tco_done = true;
                  return new Left(gRes);
                }
                ;
                $tco_var_dictApplicative = dictApplicative;
                $tco_var_fStack = fStack.value1;
                $tco_var_vals = vals;
                $copy_gVal = gRes;
                return;
              }
              ;
              if (vals instanceof Nil) {
                $tco_done = true;
                return new Left(gRes);
              }
              ;
              if (vals instanceof Cons) {
                $tco_done = true;
                return new Right(new Tuple(new Cons({
                  func: gRes,
                  count: fStack.value0.count - 1 | 0
                }, fStack.value1), new NonEmpty(vals.value0, vals.value1)));
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [vals.constructor.name]);
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [fStack.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_dictApplicative, $tco_var_fStack, $tco_var_vals, $copy_gVal);
          }
          ;
          return $tco_result;
        };
      };
    };
  };
  var functorFreeAp = {
    map: function(f) {
      return function(x) {
        return mkAp(new Pure(f))(x);
      };
    }
  };
  var foldFreeAp = function(dictApplicative) {
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply(dictApplicative)(v.value0)(v.value1.value1)(pure(dictApplicative)(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Lift) {
              var v1 = goApply(dictApplicative)(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Ap) {
              var nextVals = new NonEmpty(v.value1.value0.value1, v.value1.value1);
              $copy_v = goLeft(dictApplicative)(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [v.value1.value0.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return go2(new Tuple(Nil.value, singleton5(z)));
      };
    };
  };
  var retractFreeAp = function(dictApplicative) {
    return foldFreeAp(dictApplicative)(identity(categoryFn));
  };
  var applyFreeAp = {
    apply: function(fba) {
      return function(fb) {
        return mkAp(fba)(fb);
      };
    },
    Functor0: function() {
      return functorFreeAp;
    }
  };
  var applicativeFreeAp = /* @__PURE__ */ function() {
    return {
      pure: Pure.create,
      Apply0: function() {
        return applyFreeAp;
      }
    };
  }();
  var hoistFreeAp = function(f) {
    return foldFreeAp(applicativeFreeAp)(function($37) {
      return liftFreeAp(f($37));
    });
  };

  // output/Data.CatQueue/index.js
  var CatQueue = /* @__PURE__ */ function() {
    function CatQueue2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatQueue2.create = function(value0) {
      return function(value1) {
        return new CatQueue2(value0, value1);
      };
    };
    return CatQueue2;
  }();
  var uncons3 = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
        $tco_done = true;
        return Nothing.value;
      }
      ;
      if (v.value0 instanceof Nil) {
        $copy_v = new CatQueue(reverse2(v.value1), Nil.value);
        return;
      }
      ;
      if (v.value0 instanceof Cons) {
        $tco_done = true;
        return new Just(new Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
      }
      ;
      throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var snoc2 = function(v) {
    return function(a2) {
      return new CatQueue(v.value0, new Cons(a2, v.value1));
    };
  };
  var $$null2 = function(v) {
    if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var empty5 = /* @__PURE__ */ function() {
    return new CatQueue(Nil.value, Nil.value);
  }();

  // output/Data.CatList/index.js
  var CatNil = /* @__PURE__ */ function() {
    function CatNil2() {
    }
    ;
    CatNil2.value = new CatNil2();
    return CatNil2;
  }();
  var CatCons = /* @__PURE__ */ function() {
    function CatCons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatCons2.create = function(value0) {
      return function(value1) {
        return new CatCons2(value0, value1);
      };
    };
    return CatCons2;
  }();
  var link = function(v) {
    return function(v1) {
      if (v instanceof CatNil) {
        return v1;
      }
      ;
      if (v1 instanceof CatNil) {
        return v;
      }
      ;
      if (v instanceof CatCons) {
        return new CatCons(v.value0, snoc2(v.value1)(v1));
      }
      ;
      throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var foldr2 = function(k) {
    return function(b2) {
      return function(q2) {
        var foldl2 = function($copy_v) {
          return function($copy_c) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_var_c = $copy_c;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, c, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return c;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = v;
                  $tco_var_c = v(c)(v1.value0);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [v.constructor.name, c.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_c, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
        };
        var go2 = function($copy_xs) {
          return function($copy_ys) {
            var $tco_var_xs = $copy_xs;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(xs, ys) {
              var v = uncons3(xs);
              if (v instanceof Nothing) {
                $tco_done1 = true;
                return foldl2(function(x) {
                  return function(i2) {
                    return i2(x);
                  };
                })(b2)(ys);
              }
              ;
              if (v instanceof Just) {
                $tco_var_xs = v.value0.value1;
                $copy_ys = new Cons(k(v.value0.value0), ys);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_xs, $copy_ys);
            }
            ;
            return $tco_result;
          };
        };
        return go2(q2)(Nil.value);
      };
    };
  };
  var uncons4 = function(v) {
    if (v instanceof CatNil) {
      return Nothing.value;
    }
    ;
    if (v instanceof CatCons) {
      return new Just(new Tuple(v.value0, function() {
        var $45 = $$null2(v.value1);
        if ($45) {
          return CatNil.value;
        }
        ;
        return foldr2(link)(CatNil.value)(v.value1);
      }()));
    }
    ;
    throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [v.constructor.name]);
  };
  var empty6 = /* @__PURE__ */ function() {
    return CatNil.value;
  }();
  var append2 = link;
  var semigroupCatList = {
    append: append2
  };
  var snoc3 = function(cat) {
    return function(a2) {
      return append2(cat)(new CatCons(a2, empty5));
    };
  };

  // output/Control.Monad.Free/index.js
  var $runtime_lazy6 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Free = /* @__PURE__ */ function() {
    function Free2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Free2.create = function(value0) {
      return function(value1) {
        return new Free2(value0, value1);
      };
    };
    return Free2;
  }();
  var Return = /* @__PURE__ */ function() {
    function Return2(value0) {
      this.value0 = value0;
    }
    ;
    Return2.create = function(value0) {
      return new Return2(value0);
    };
    return Return2;
  }();
  var Bind = /* @__PURE__ */ function() {
    function Bind2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Bind2.create = function(value0) {
      return function(value1) {
        return new Bind2(value0, value1);
      };
    };
    return Bind2;
  }();
  var toView = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      var runExpF = function(v22) {
        return v22;
      };
      var concatF = function(v22) {
        return function(r) {
          return new Free(v22.value0, append(semigroupCatList)(v22.value1)(r));
        };
      };
      if (v.value0 instanceof Return) {
        var v2 = uncons4(v.value1);
        if (v2 instanceof Nothing) {
          $tco_done = true;
          return new Return(v.value0.value0);
        }
        ;
        if (v2 instanceof Just) {
          $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
          return;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [v2.constructor.name]);
      }
      ;
      if (v.value0 instanceof Bind) {
        $tco_done = true;
        return new Bind(v.value0.value0, function(a2) {
          return concatF(v.value0.value1(a2))(v.value1);
        });
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [v.value0.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var fromView = function(f) {
    return new Free(f, empty6);
  };
  var freeMonad = {
    Applicative0: function() {
      return freeApplicative;
    },
    Bind1: function() {
      return freeBind;
    }
  };
  var freeFunctor = {
    map: function(k) {
      return function(f) {
        return bindFlipped(freeBind)(function() {
          var $119 = pure(freeApplicative);
          return function($120) {
            return $119(k($120));
          };
        }())(f);
      };
    }
  };
  var freeBind = {
    bind: function(v) {
      return function(k) {
        return new Free(v.value0, snoc3(v.value1)(k));
      };
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var freeApplicative = {
    pure: function($121) {
      return fromView(Return.create($121));
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy6("freeApply", "Control.Monad.Free", function() {
    return {
      apply: ap(freeMonad),
      Functor0: function() {
        return freeFunctor;
      }
    };
  });
  var liftF = function(f) {
    return fromView(new Bind(f, function() {
      var $122 = pure(freeApplicative);
      return function($123) {
        return $122($123);
      };
    }()));
  };
  var foldFree = function(dictMonadRec) {
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(Done.create)(pure(dictMonadRec.Monad0().Applicative0())(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(function($135) {
            return Loop.create(v.value1($135));
          })(k(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
      };
      return tailRecM(dictMonadRec)(go2);
    };
  };

  // output/Effect.Aff.Class/index.js
  var monadAffAff = {
    liftAff: /* @__PURE__ */ identity(categoryFn),
    MonadEffect0: function() {
      return monadEffectAff;
    }
  };
  var liftAff = function(dict) {
    return dict.liftAff;
  };

  // output/Halogen.Query.ChildQuery/index.js
  var ChildQuery = /* @__PURE__ */ function() {
    function ChildQuery3(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ChildQuery3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ChildQuery3(value0, value1, value22);
        };
      };
    };
    return ChildQuery3;
  }();
  var unChildQueryBox = unsafeCoerce2;
  var mkChildQueryBox = unsafeCoerce2;

  // output/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a2) {
    return function(b2) {
      return a2 === b2;
    };
  }

  // output/Unsafe.Reference/index.js
  var unsafeRefEq = reallyUnsafeRefEq;

  // output/Halogen.Subscription/index.js
  var unsubscribe = function(v) {
    return v;
  };
  var subscribe = function(v) {
    return function(k) {
      return v(function() {
        var $55 = $$void(functorEffect);
        return function($56) {
          return $55(k($56));
        };
      }());
    };
  };
  var notify = function(v) {
    return function(a2) {
      return v(a2);
    };
  };
  var create = function __do() {
    var subscribers = $$new([])();
    return {
      emitter: function(k) {
        return function __do2() {
          modify_2(function(v) {
            return append(semigroupArray)(v)([k]);
          })(subscribers)();
          return modify_2(deleteBy(unsafeRefEq)(k))(subscribers);
        };
      },
      listener: function(a2) {
        return bind(bindEffect)(read(subscribers))(traverse_(applicativeEffect)(foldableArray)(function(k) {
          return k(a2);
        }));
      }
    };
  };

  // output/Halogen.Query.HalogenM/index.js
  var SubscriptionId = function(x) {
    return x;
  };
  var ForkId = function(x) {
    return x;
  };
  var State = /* @__PURE__ */ function() {
    function State2(value0) {
      this.value0 = value0;
    }
    ;
    State2.create = function(value0) {
      return new State2(value0);
    };
    return State2;
  }();
  var Subscribe = /* @__PURE__ */ function() {
    function Subscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Subscribe2.create = function(value0) {
      return function(value1) {
        return new Subscribe2(value0, value1);
      };
    };
    return Subscribe2;
  }();
  var Unsubscribe = /* @__PURE__ */ function() {
    function Unsubscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Unsubscribe2.create = function(value0) {
      return function(value1) {
        return new Unsubscribe2(value0, value1);
      };
    };
    return Unsubscribe2;
  }();
  var Lift2 = /* @__PURE__ */ function() {
    function Lift3(value0) {
      this.value0 = value0;
    }
    ;
    Lift3.create = function(value0) {
      return new Lift3(value0);
    };
    return Lift3;
  }();
  var ChildQuery2 = /* @__PURE__ */ function() {
    function ChildQuery3(value0) {
      this.value0 = value0;
    }
    ;
    ChildQuery3.create = function(value0) {
      return new ChildQuery3(value0);
    };
    return ChildQuery3;
  }();
  var Raise = /* @__PURE__ */ function() {
    function Raise3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Raise3.create = function(value0) {
      return function(value1) {
        return new Raise3(value0, value1);
      };
    };
    return Raise3;
  }();
  var Par = /* @__PURE__ */ function() {
    function Par2(value0) {
      this.value0 = value0;
    }
    ;
    Par2.create = function(value0) {
      return new Par2(value0);
    };
    return Par2;
  }();
  var Fork = /* @__PURE__ */ function() {
    function Fork2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Fork2.create = function(value0) {
      return function(value1) {
        return new Fork2(value0, value1);
      };
    };
    return Fork2;
  }();
  var Join = /* @__PURE__ */ function() {
    function Join2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Join2.create = function(value0) {
      return function(value1) {
        return new Join2(value0, value1);
      };
    };
    return Join2;
  }();
  var Kill = /* @__PURE__ */ function() {
    function Kill2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Kill2.create = function(value0) {
      return function(value1) {
        return new Kill2(value0, value1);
      };
    };
    return Kill2;
  }();
  var GetRef = /* @__PURE__ */ function() {
    function GetRef2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    GetRef2.create = function(value0) {
      return function(value1) {
        return new GetRef2(value0, value1);
      };
    };
    return GetRef2;
  }();
  var HalogenM = function(x) {
    return x;
  };
  var raise = function(o) {
    return liftF(new Raise(o, unit));
  };
  var query = function() {
    return function(dictIsSymbol) {
      return function(dictOrd) {
        return function(label5) {
          return function(p2) {
            return function(q2) {
              return liftF(new ChildQuery2(mkChildQueryBox(new ChildQuery(function(dictApplicative) {
                return function(k) {
                  var $140 = maybe(pure(dictApplicative)(Nothing.value))(k);
                  var $141 = lookup4()(dictIsSymbol)(dictOrd)(label5)(p2);
                  return function($142) {
                    return $140($141($142));
                  };
                };
              }, q2, identity(categoryFn)))));
            };
          };
        };
      };
    };
  };
  var ordSubscriptionId = ordInt;
  var ordForkId = ordInt;
  var monadHalogenM = freeMonad;
  var monadStateHalogenM = {
    state: function($144) {
      return HalogenM(liftF(State.create($144)));
    },
    Monad0: function() {
      return monadHalogenM;
    }
  };
  var monadEffectHalogenM = function(dictMonadEffect) {
    return {
      liftEffect: function() {
        var $149 = liftEffect(dictMonadEffect);
        return function($150) {
          return HalogenM(liftF(Lift2.create($149($150))));
        };
      }(),
      Monad0: function() {
        return monadHalogenM;
      }
    };
  };
  var monadAffHalogenM = function(dictMonadAff) {
    return {
      liftAff: function() {
        var $151 = liftAff(dictMonadAff);
        return function($152) {
          return HalogenM(liftF(Lift2.create($151($152))));
        };
      }(),
      MonadEffect0: function() {
        return monadEffectHalogenM(dictMonadAff.MonadEffect0());
      }
    };
  };
  var functorHalogenM = freeFunctor;
  var fork = function(hmu) {
    return liftF(new Fork(hmu, identity(categoryFn)));
  };
  var bindHalogenM = freeBind;
  var applicativeHalogenM = freeApplicative;

  // output/Halogen.Query.HalogenQ/index.js
  var Initialize = /* @__PURE__ */ function() {
    function Initialize4(value0) {
      this.value0 = value0;
    }
    ;
    Initialize4.create = function(value0) {
      return new Initialize4(value0);
    };
    return Initialize4;
  }();
  var Finalize = /* @__PURE__ */ function() {
    function Finalize2(value0) {
      this.value0 = value0;
    }
    ;
    Finalize2.create = function(value0) {
      return new Finalize2(value0);
    };
    return Finalize2;
  }();
  var Receive = /* @__PURE__ */ function() {
    function Receive8(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Receive8.create = function(value0) {
      return function(value1) {
        return new Receive8(value0, value1);
      };
    };
    return Receive8;
  }();
  var Action2 = /* @__PURE__ */ function() {
    function Action3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Action3.create = function(value0) {
      return function(value1) {
        return new Action3(value0, value1);
      };
    };
    return Action3;
  }();
  var Query = /* @__PURE__ */ function() {
    function Query4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Query4.create = function(value0) {
      return function(value1) {
        return new Query4(value0, value1);
      };
    };
    return Query4;
  }();

  // output/Halogen.VDom.Thunk/index.js
  var $runtime_lazy7 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var unsafeEqThunk = function(v, v1) {
    return refEq2(v.value0, v1.value0) && (refEq2(v.value1, v1.value1) && v.value1(v.value3, v1.value3));
  };
  var runThunk = function(v) {
    return v.value2(v.value3);
  };
  var buildThunk = function(toVDom) {
    var haltThunk = function(state3) {
      return halt(state3.vdom);
    };
    var $lazy_patchThunk = $runtime_lazy7("patchThunk", "Halogen.VDom.Thunk", function() {
      return function(state3, t2) {
        var $43 = unsafeEqThunk(state3.thunk, t2);
        if ($43) {
          return mkStep(new Step(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
        }
        ;
        var vdom = step2(state3.vdom, toVDom(runThunk(t2)));
        return mkStep(new Step(extract2(vdom), {
          vdom,
          thunk: t2
        }, $lazy_patchThunk(115), haltThunk));
      };
    });
    var patchThunk = $lazy_patchThunk(108);
    var renderThunk = function(spec) {
      return function(t) {
        var vdom = buildVDom(spec)(toVDom(runThunk(t)));
        return mkStep(new Step(extract2(vdom), {
          thunk: t,
          vdom
        }, patchThunk, haltThunk));
      };
    };
    return renderThunk;
  };

  // output/Halogen.Component/index.js
  var ComponentSlot = /* @__PURE__ */ function() {
    function ComponentSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ComponentSlot2.create = function(value0) {
      return new ComponentSlot2(value0);
    };
    return ComponentSlot2;
  }();
  var ThunkSlot = /* @__PURE__ */ function() {
    function ThunkSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkSlot2.create = function(value0) {
      return new ThunkSlot2(value0);
    };
    return ThunkSlot2;
  }();
  var unComponentSlot = unsafeCoerce2;
  var unComponent = unsafeCoerce2;
  var mkEval = function(args) {
    return function(v) {
      if (v instanceof Initialize) {
        return voidLeft(functorHalogenM)(traverse_(applicativeHalogenM)(foldableMaybe)(args.handleAction)(args.initialize))(v.value0);
      }
      ;
      if (v instanceof Finalize) {
        return voidLeft(functorHalogenM)(traverse_(applicativeHalogenM)(foldableMaybe)(args.handleAction)(args.finalize))(v.value0);
      }
      ;
      if (v instanceof Receive) {
        return voidLeft(functorHalogenM)(traverse_(applicativeHalogenM)(foldableMaybe)(args.handleAction)(args.receive(v.value0)))(v.value1);
      }
      ;
      if (v instanceof Action2) {
        return voidLeft(functorHalogenM)(args.handleAction(v.value0))(v.value1);
      }
      ;
      if (v instanceof Query) {
        return unCoyoneda(function(g) {
          var $24 = map(functorHalogenM)(maybe(v.value1(unit))(g));
          return function($25) {
            return $24(args.handleQuery($25));
          };
        })(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Halogen.Component (line 182, column 15 - line 192, column 71): " + [v.constructor.name]);
    };
  };
  var mkComponentSlot = unsafeCoerce2;
  var mkComponent = unsafeCoerce2;
  var defaultEval = /* @__PURE__ */ function() {
    return {
      handleAction: $$const(pure(applicativeHalogenM)(unit)),
      handleQuery: $$const(pure(applicativeHalogenM)(Nothing.value)),
      receive: $$const(Nothing.value),
      initialize: Nothing.value,
      finalize: Nothing.value
    };
  }();
  var componentSlot = function() {
    return function(dictIsSymbol) {
      return function(dictOrd) {
        return function(label5) {
          return function(p2) {
            return function(comp) {
              return function(input3) {
                return function(output2) {
                  return mkComponentSlot({
                    get: lookup4()(dictIsSymbol)(dictOrd)(label5)(p2),
                    pop: pop2()(dictIsSymbol)(dictOrd)(label5)(p2),
                    set: insert5()(dictIsSymbol)(dictOrd)(label5)(p2),
                    component: comp,
                    input: input3,
                    output: output2
                  });
                };
              };
            };
          };
        };
      };
    };
  };

  // output/Halogen.HTML.Elements/index.js
  var element2 = /* @__PURE__ */ function() {
    return element(Nothing.value);
  }();
  var fieldset = /* @__PURE__ */ element2("fieldset");
  var fieldset_ = /* @__PURE__ */ fieldset([]);
  var form = /* @__PURE__ */ element2("form");
  var h1 = /* @__PURE__ */ element2("h1");
  var h1_ = /* @__PURE__ */ h1([]);
  var h2 = /* @__PURE__ */ element2("h2");
  var h2_ = /* @__PURE__ */ h2([]);
  var input = function(props) {
    return element2("input")(props)([]);
  };
  var label = /* @__PURE__ */ element2("label");
  var label_ = /* @__PURE__ */ label([]);
  var li = /* @__PURE__ */ element2("li");
  var li_ = /* @__PURE__ */ li([]);
  var p = /* @__PURE__ */ element2("p");
  var p_ = /* @__PURE__ */ p([]);
  var small = /* @__PURE__ */ element2("small");
  var small_ = /* @__PURE__ */ small([]);
  var textarea = function(es) {
    return element2("textarea")(es)([]);
  };
  var ul = /* @__PURE__ */ element2("ul");
  var div2 = /* @__PURE__ */ element2("div");
  var div_ = /* @__PURE__ */ div2([]);
  var code = /* @__PURE__ */ element2("code");
  var code_ = /* @__PURE__ */ code([]);
  var button = /* @__PURE__ */ element2("button");
  var br = function(props) {
    return element2("br")(props)([]);
  };
  var br_ = /* @__PURE__ */ br([]);
  var article = /* @__PURE__ */ element2("article");
  var article_ = /* @__PURE__ */ article([]);
  var a = /* @__PURE__ */ element2("a");

  // output/Halogen.HTML.Properties/index.js
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var type_ = function(dictIsProp) {
    return prop2(dictIsProp)("type");
  };
  var value = function(dictIsProp) {
    return prop2(dictIsProp)("value");
  };
  var placeholder = /* @__PURE__ */ prop2(isPropString)("placeholder");
  var name2 = /* @__PURE__ */ prop2(isPropString)("name");
  var href = /* @__PURE__ */ prop2(isPropString)("href");
  var $$for = /* @__PURE__ */ prop2(isPropString)("htmlFor");
  var class_ = /* @__PURE__ */ function() {
    var $14 = prop2(isPropString)("className");
    var $15 = unwrap();
    return function($16) {
      return $14($15($16));
    };
  }();
  var checked = /* @__PURE__ */ prop2(isPropBoolean)("checked");
  var attr2 = /* @__PURE__ */ function() {
    return attr(Nothing.value);
  }();

  // output/Halogen.HTML/index.js
  var slot_ = function() {
    return function(dictIsSymbol) {
      return function(dictOrd) {
        return function(label5) {
          return function(p2) {
            return function(component) {
              return function(input3) {
                return widget(new ComponentSlot(componentSlot()(dictIsSymbol)(dictOrd)(label5)(p2)(component)(input3)($$const(Nothing.value))));
              };
            };
          };
        };
      };
    };
  };
  var slot = function() {
    return function(dictIsSymbol) {
      return function(dictOrd) {
        return function(label5) {
          return function(p2) {
            return function(component) {
              return function(input3) {
                return function(outputQuery) {
                  return widget(new ComponentSlot(componentSlot()(dictIsSymbol)(dictOrd)(label5)(p2)(component)(input3)(function($4) {
                    return Just.create(outputQuery($4));
                  })));
                };
              };
            };
          };
        };
      };
    };
  };
  var fromPlainHTML = unsafeCoerce2;

  // output/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value13) {
    var tag = Object.prototype.toString.call(value13);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value13);
    } else {
      return nothing;
    }
  }

  // output/Web.HTML.HTMLElement/index.js
  var toNode2 = unsafeCoerce2;
  var fromElement = function(x) {
    return _read(Nothing.value, Just.create, x);
  };

  // output/Halogen.Query/index.js
  var tell2 = function() {
    return function(dictIsSymbol) {
      return function(dictOrd) {
        return function(slot2) {
          return function(label5) {
            return function(req) {
              return $$void(functorHalogenM)(query()(dictIsSymbol)(dictOrd)(slot2)(label5)(req(unit)));
            };
          };
        };
      };
    };
  };
  var request = function() {
    return function(dictIsSymbol) {
      return function(dictOrd) {
        return function(slot2) {
          return function(label5) {
            return function(req) {
              return query()(dictIsSymbol)(dictOrd)(slot2)(label5)(req(identity(categoryFn)));
            };
          };
        };
      };
    };
  };
  var mkTell = function(act) {
    return act(unit);
  };

  // output/Heterogeneous.Folding/index.js
  var hfoldlWithIndex = function(dict) {
    return dict.hfoldlWithIndex;
  };
  var foldlRecordRowList = function(dict) {
    return dict.foldlRecordRowList;
  };
  var hfoldlRecordWithIndex = function() {
    return function(dictFoldlRecord) {
      return {
        hfoldlWithIndex: function(f) {
          return function(x) {
            return foldlRecordRowList(dictFoldlRecord)(f)(x)($$Proxy.value);
          };
        }
      };
    };
  };
  var foldlRecordNil = {
    foldlRecordRowList: function(v) {
      return function(x) {
        return function(v1) {
          return function(v2) {
            return x;
          };
        };
      };
    }
  };
  var foldingWithIndex = function(dict) {
    return dict.foldingWithIndex;
  };
  var foldlRecordCons = function(dictIsSymbol) {
    return function() {
      return function(dictFoldingWithIndex) {
        return function(dictFoldlRecord) {
          return {
            foldlRecordRowList: function(f) {
              return function(x) {
                return function(v) {
                  return function(r) {
                    return foldlRecordRowList(dictFoldlRecord)(f)(foldingWithIndex(dictFoldingWithIndex)(f)($$Proxy.value)(x)(get2(dictIsSymbol)()($$Proxy.value)(r)))($$Proxy.value)(r);
                  };
                };
              };
            }
          };
        };
      };
    };
  };

  // output/Heterogeneous.Mapping/index.js
  var ConstMapping = function(x) {
    return x;
  };
  var mappingWithIndex = function(dict) {
    return dict.mappingWithIndex;
  };
  var mapping = function(dict) {
    return dict.mapping;
  };
  var mapRecordWithIndexNil = {
    mapRecordWithIndexBuilder: function(v) {
      return function(v1) {
        return identity(categoryBuilder);
      };
    }
  };
  var mapRecordWithIndexBuilder = function(dict) {
    return dict.mapRecordWithIndexBuilder;
  };
  var mapRecordWithIndexCons = function(dictIsSymbol) {
    return function(dictMappingWithIndex) {
      return function(dictMapRecordWithIndex) {
        return function() {
          return function() {
            return {
              mapRecordWithIndexBuilder: function(v) {
                return function(f) {
                  return compose(semigroupoidBuilder)(modify3()()(dictIsSymbol)($$Proxy.value)(mappingWithIndex(dictMappingWithIndex)(f)($$Proxy.value)))(mapRecordWithIndexBuilder(dictMapRecordWithIndex)($$Proxy.value)(f));
                };
              }
            };
          };
        };
      };
    };
  };
  var hmapWithIndexRecord = function() {
    return function(dictMapRecordWithIndex) {
      return {
        hmapWithIndex: function() {
          var $66 = mapRecordWithIndexBuilder(dictMapRecordWithIndex)($$Proxy.value);
          return function($67) {
            return build($66($67));
          };
        }()
      };
    };
  };
  var hmapWithIndex = function(dict) {
    return dict.hmapWithIndex;
  };
  var hmapRecord = function() {
    return function(dictMapRecordWithIndex) {
      return {
        hmap: function() {
          var $72 = mapRecordWithIndexBuilder(dictMapRecordWithIndex)($$Proxy.value);
          return function($73) {
            return build($72(ConstMapping($73)));
          };
        }()
      };
    };
  };
  var hmap = function(dict) {
    return dict.hmap;
  };
  var constMapping = function(dictMapping) {
    return {
      mappingWithIndex: function(v) {
        return function(v1) {
          return mapping(dictMapping)(v);
        };
      }
    };
  };

  // output/Web.Event.Event/foreign.js
  function _currentTarget(e) {
    return e.currentTarget;
  }
  function _target(e) {
    return e.target;
  }
  function preventDefault(e) {
    return function() {
      return e.preventDefault();
    };
  }

  // output/Web.Event.Event/index.js
  var target = function($0) {
    return toMaybe(_target($0));
  };
  var currentTarget = function($4) {
    return toMaybe(_currentTarget($4));
  };

  // output/Formless/index.js
  var MkFieldState = /* @__PURE__ */ function() {
    function MkFieldState2() {
    }
    ;
    MkFieldState2.value = new MkFieldState2();
    return MkFieldState2;
  }();
  var MkFieldResult = /* @__PURE__ */ function() {
    function MkFieldResult2() {
    }
    ;
    MkFieldResult2.value = new MkFieldResult2();
    return MkFieldResult2;
  }();
  var MkFieldOutput = /* @__PURE__ */ function() {
    function MkFieldOutput2() {
    }
    ;
    MkFieldOutput2.value = new MkFieldOutput2();
    return MkFieldOutput2;
  }();
  var Query2 = /* @__PURE__ */ function() {
    function Query4(value0) {
      this.value0 = value0;
    }
    ;
    Query4.create = function(value0) {
      return new Query4(value0);
    };
    return Query4;
  }();
  var Validate = /* @__PURE__ */ function() {
    function Validate2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Validate2.create = function(value0) {
      return function(value1) {
        return new Validate2(value0, value1);
      };
    };
    return Validate2;
  }();
  var SubmitAttempt = /* @__PURE__ */ function() {
    function SubmitAttempt2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SubmitAttempt2.create = function(value0) {
      return function(value1) {
        return new SubmitAttempt2(value0, value1);
      };
    };
    return SubmitAttempt2;
  }();
  var Submit = /* @__PURE__ */ function() {
    function Submit2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Submit2.create = function(value0) {
      return function(value1) {
        return new Submit2(value0, value1);
      };
    };
    return Submit2;
  }();
  var Reset = /* @__PURE__ */ function() {
    function Reset2(value0) {
      this.value0 = value0;
    }
    ;
    Reset2.create = function(value0) {
      return new Reset2(value0);
    };
    return Reset2;
  }();
  var SubmitForm = /* @__PURE__ */ function() {
    function SubmitForm2(value0) {
      this.value0 = value0;
    }
    ;
    SubmitForm2.create = function(value0) {
      return new SubmitForm2(value0);
    };
    return SubmitForm2;
  }();
  var ResetForm = /* @__PURE__ */ function() {
    function ResetForm2() {
    }
    ;
    ResetForm2.value = new ResetForm2();
    return ResetForm2;
  }();
  var SetForm = /* @__PURE__ */ function() {
    function SetForm2(value0) {
      this.value0 = value0;
    }
    ;
    SetForm2.create = function(value0) {
      return new SetForm2(value0);
    };
    return SetForm2;
  }();
  var SetFormConfig = /* @__PURE__ */ function() {
    function SetFormConfig2(value0) {
      this.value0 = value0;
    }
    ;
    SetFormConfig2.create = function(value0) {
      return new SetFormConfig2(value0);
    };
    return SetFormConfig2;
  }();
  var ChangeField = /* @__PURE__ */ function() {
    function ChangeField2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ChangeField2.create = function(value0) {
      return function(value1) {
        return new ChangeField2(value0, value1);
      };
    };
    return ChangeField2;
  }();
  var BlurField = /* @__PURE__ */ function() {
    function BlurField2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    BlurField2.create = function(value0) {
      return function(value1) {
        return new BlurField2(value0, value1);
      };
    };
    return BlurField2;
  }();
  var ModifyField = /* @__PURE__ */ function() {
    function ModifyField2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ModifyField2.create = function(value0) {
      return function(value1) {
        return new ModifyField2(value0, value1);
      };
    };
    return ModifyField2;
  }();
  var ValidateField = /* @__PURE__ */ function() {
    function ValidateField2(value0) {
      this.value0 = value0;
    }
    ;
    ValidateField2.create = function(value0) {
      return new ValidateField2(value0);
    };
    return ValidateField2;
  }();
  var ResetField = /* @__PURE__ */ function() {
    function ResetField2(value0) {
      this.value0 = value0;
    }
    ;
    ResetField2.create = function(value0) {
      return new ResetField2(value0);
    };
    return ResetField2;
  }();
  var Raise2 = /* @__PURE__ */ function() {
    function Raise3(value0) {
      this.value0 = value0;
    }
    ;
    Raise3.create = function(value0) {
      return new Raise3(value0);
    };
    return Raise3;
  }();
  var Eval = /* @__PURE__ */ function() {
    function Eval7(value0) {
      this.value0 = value0;
    }
    ;
    Eval7.create = function(value0) {
      return new Eval7(value0);
    };
    return Eval7;
  }();
  var Initialize2 = /* @__PURE__ */ function() {
    function Initialize4() {
    }
    ;
    Initialize4.value = new Initialize4();
    return Initialize4;
  }();
  var Receive2 = /* @__PURE__ */ function() {
    function Receive8(value0) {
      this.value0 = value0;
    }
    ;
    Receive8.create = function(value0) {
      return new Receive8(value0);
    };
    return Receive8;
  }();
  var HandleForm = /* @__PURE__ */ function() {
    function HandleForm2(value0) {
      this.value0 = value0;
    }
    ;
    HandleForm2.create = function(value0) {
      return new HandleForm2(value0);
    };
    return HandleForm2;
  }();
  var mkFieldStates1 = function(dictHMap) {
    return {
      mkFieldStates: hmap(dictHMap)(MkFieldState.value),
      HMap0: function() {
        return dictHMap;
      }
    };
  };
  var mkFieldResults1 = function(dictHFoldlWithIndex) {
    return {
      mkFieldResults: function() {
        var $195 = map(functorMaybe)(flip(build)({}));
        var $196 = hfoldlWithIndex(dictHFoldlWithIndex)(MkFieldResult.value)(pure(applicativeMaybe)(identity(categoryBuilder)));
        return function($197) {
          return $195($196($197));
        };
      }(),
      HFoldlWithIndex0: function() {
        return dictHFoldlWithIndex;
      }
    };
  };
  var mkFieldOutputs1 = function(dictHFoldlWithIndex) {
    return {
      mkFieldOutputs: function() {
        var $198 = map(functorMaybe)(flip(build)({}));
        var $199 = hfoldlWithIndex(dictHFoldlWithIndex)(MkFieldOutput.value)(pure(applicativeMaybe)(identity(categoryBuilder)));
        return function($200) {
          return $198($199($200));
        };
      }(),
      HFoldlWithIndex0: function() {
        return dictHFoldlWithIndex;
      }
    };
  };
  var mkFieldActions1 = function(dictHMapWithIndex) {
    return {
      mkFieldActions: function(lift3) {
        return hmapWithIndex(dictHMapWithIndex)(lift3);
      },
      HMapWithIndex0: function() {
        return dictHMapWithIndex;
      }
    };
  };
  var mappingWithIndexMkFieldAc = function(dictIsSymbol) {
    return function(dictTypeEquals) {
      return function() {
        return {
          mappingWithIndex: function(v) {
            return function(sym) {
              return function(v1) {
                var fieldVariant = inj()(dictIsSymbol)(sym)(unit);
                return {
                  key: reflectSymbol(dictIsSymbol)(sym),
                  modify: function() {
                    var $201 = ModifyField.create(fieldVariant);
                    return function($202) {
                      return v($201($202));
                    };
                  }(),
                  reset: v(new ResetField(fieldVariant)),
                  validate: v(new ValidateField(fieldVariant)),
                  handleChange: function() {
                    var $203 = ChangeField.create(fieldVariant);
                    return function($204) {
                      return v($203($204));
                    };
                  }(),
                  handleBlur: function() {
                    var $205 = BlurField.create(fieldVariant);
                    return function($206) {
                      return v($205($206));
                    };
                  }()
                };
              };
            };
          }
        };
      };
    };
  };
  var mappingMkFieldStateFieldS = {
    mapping: function(v) {
      return function(input3) {
        return {
          initialValue: input3,
          value: input3,
          result: Nothing.value
        };
      };
    }
  };
  var foldingWithIndexMkFieldRe = function(dictIsSymbol) {
    return function(dictTypeEquals) {
      return function() {
        return function() {
          return {
            foldingWithIndex: function(v) {
              return function(prop3) {
                return function(rin) {
                  return function(field) {
                    var v1 = from(dictTypeEquals)(field);
                    return apply(applyMaybe)(map(functorMaybe)(composeFlipped(semigroupoidBuilder))(rin))(map(functorMaybe)(insert2()()(dictIsSymbol)(prop3))(v1.result));
                  };
                };
              };
            }
          };
        };
      };
    };
  };
  var foldingWithIndexMkFieldOu = function(dictIsSymbol) {
    return function(dictTypeEquals) {
      return function() {
        return function() {
          return {
            foldingWithIndex: function(v) {
              return function(prop3) {
                return function(rin) {
                  return function(field) {
                    var result = from(dictTypeEquals)(field);
                    return apply(applyMaybe)(map(functorMaybe)(composeFlipped(semigroupoidBuilder))(rin))(map(functorMaybe)(insert2()()(dictIsSymbol)(prop3))(hush(result)));
                  };
                };
              };
            }
          };
        };
      };
    };
  };
  var when$prime = function(dictApplicative) {
    return function(v) {
      return function(v1) {
        if (v) {
          return v1(unit);
        }
        ;
        return pure(dictApplicative)(unit);
      };
    };
  };
  var validateM = function() {
    return function() {
      return function() {
        return function() {
          return function(dictApplicative) {
            return flip(traverse2()()()()(dictApplicative));
          };
        };
      };
    };
  };
  var validate = function() {
    return function() {
      return function() {
        return function() {
          return flip(over2()()()());
        };
      };
    };
  };
  var raise2 = function($207) {
    return raise(Raise2.create($207));
  };
  var mkFieldStates = function(dict) {
    return dict.mkFieldStates;
  };
  var mkFieldResults = function(dict) {
    return dict.mkFieldResults;
  };
  var mkFieldOutputs = function(dict) {
    return dict.mkFieldOutputs;
  };
  var mkFieldActions = function(dict) {
    return dict.mkFieldActions;
  };
  var mkConfig = function(dict) {
    return dict.mkConfig;
  };
  var handleSubmitValidateM = function(onSubmit2) {
    return function(validateM$prime) {
      return function(validators) {
        return function(v) {
          if (v instanceof Submit) {
            return discard(discardUnit)(bindHalogenM)(onSubmit2(v.value0))(function() {
              return pure(applicativeHalogenM)(new Just(v.value1));
            });
          }
          ;
          if (v instanceof Validate) {
            return bind(bindHalogenM)(validateM$prime(v.value0)(validators))(function(validated) {
              return pure(applicativeHalogenM)(new Just(v.value1(validated)));
            });
          }
          ;
          return pure(applicativeHalogenM)(Nothing.value);
        };
      };
    };
  };
  var handleSubmitValidate = function(onSubmit2) {
    return function(validate$prime) {
      return function(validators) {
        return function(v) {
          if (v instanceof Submit) {
            return discard(discardUnit)(bindHalogenM)(onSubmit2(v.value0))(function() {
              return pure(applicativeHalogenM)(new Just(v.value1));
            });
          }
          ;
          if (v instanceof Validate) {
            return pure(applicativeHalogenM)(new Just(v.value1(validate$prime(v.value0)(validators))));
          }
          ;
          return pure(applicativeHalogenM)(Nothing.value);
        };
      };
    };
  };
  var formless = function(dictMonadEffect) {
    return function(dictMkFieldStates) {
      return function(dictMkFieldActions) {
        return function(dictMkFieldResults) {
          return function(dictMkFieldOutputs) {
            return function(dictMkConfig) {
              return function(providedConfig) {
                return function(initialForm) {
                  return function(component) {
                    var setFieldResult = function(v) {
                      return function(object2) {
                        var field = unsafeIndex2(object2)(v.type);
                        return insert3(v.type)({
                          result: new Just(v.value),
                          initialValue: field.initialValue,
                          value: field.value
                        })(object2);
                      };
                    };
                    var modifyField = function(v) {
                      return function(object2) {
                        var field = unsafeIndex2(object2)(v.type);
                        return insert3(v.type)(v.value(field))(object2);
                      };
                    };
                    var initialState = function(input3) {
                      var initialFullConfig = mkConfig(dictMkConfig)(providedConfig);
                      var initialFormState = {
                        submitCount: 0,
                        errorCount: 0,
                        allTouched: false
                      };
                      var initialFormActions = {
                        setFields: function($208) {
                          return initialFullConfig.liftAction(SetForm.create($208));
                        },
                        reset: initialFullConfig.liftAction(ResetForm.value),
                        setConfig: function($209) {
                          return initialFullConfig.liftAction(SetFormConfig.create($209));
                        },
                        submit: initialFullConfig.liftAction(new SubmitForm(Nothing.value)),
                        handleSubmit: function($210) {
                          return initialFullConfig.liftAction(SubmitForm.create(Just.create($210)));
                        }
                      };
                      var initialFieldStates = mkFieldStates(dictMkFieldStates)(initialForm);
                      var initialFieldActions = mkFieldActions(dictMkFieldActions)(initialFullConfig.liftAction)(initialFieldStates);
                      var initialConfig = $$delete({
                        reflectSymbol: function() {
                          return "liftAction";
                        }
                      })()()($$Proxy.value)(initialFullConfig);
                      return {
                        input: input3,
                        fieldObject: initialFieldStates,
                        fieldActions: initialFieldActions,
                        formState: initialFormState,
                        formActions: initialFormActions,
                        formConfig: initialConfig
                      };
                    };
                    var getKeys = function() {
                      var $211 = coerce();
                      return function($212) {
                        return $211(keys($212));
                      };
                    }();
                    var getField = function(v) {
                      return function(object2) {
                        var field = unsafeIndex2(object2)(v);
                        return {
                          type: v,
                          value: field.value
                        };
                      };
                    };
                    var fieldsKey = function($213) {
                      return coerce()($213.type);
                    };
                    var mkFieldRep = function(variant) {
                      return function(value13) {
                        return {
                          type: coerce()(fieldsKey(variant)),
                          value: value13
                        };
                      };
                    };
                    var countErrors = fold2(function(acc) {
                      return function(v) {
                        return function(v1) {
                          if (v1.result instanceof Just && v1.result.value0 instanceof Left) {
                            return acc + 1 | 0;
                          }
                          ;
                          return acc;
                        };
                      };
                    })(0);
                    var allTouched = fold2(function(acc) {
                      return function(v) {
                        return function(v1) {
                          if (v1.result instanceof Just) {
                            return acc && true;
                          }
                          ;
                          return false;
                        };
                      };
                    })(true);
                    var runValidation = function(fieldKey) {
                      return bind(bindHalogenM)(bindFlipped(bindHalogenM)(function() {
                        var $214 = request()({
                          reflectSymbol: function() {
                            return "inner";
                          }
                        })(ordUnit)($$Proxy.value)(unit);
                        var $215 = getField(fieldKey);
                        return function($216) {
                          return $214(Validate.create($215($216)));
                        };
                      }())(gets(monadStateHalogenM)(function(v) {
                        return v.fieldObject;
                      })))(function(mbResult) {
                        return for_(applicativeHalogenM)(foldableMaybe)(mbResult)(function(resultVariant) {
                          return modify_(monadStateHalogenM)(function(state3) {
                            var fieldObject = setFieldResult(resultVariant)(state3.fieldObject);
                            var $127 = {};
                            for (var $128 in state3) {
                              if ({}.hasOwnProperty.call(state3, $128)) {
                                $127[$128] = state3[$128];
                              }
                              ;
                            }
                            ;
                            $127.fieldObject = fieldObject;
                            $127.formState = function() {
                              var $124 = {};
                              for (var $125 in state3.formState) {
                                if ({}.hasOwnProperty.call(state3.formState, $125)) {
                                  $124[$125] = state3["formState"][$125];
                                }
                                ;
                              }
                              ;
                              $124.errorCount = countErrors(fieldObject);
                              $124.allTouched = allTouched(fieldObject);
                              return $124;
                            }();
                            return $127;
                          });
                        });
                      });
                    };
                    var runFormAction = function(action2) {
                      return handleAction(new HandleForm(new Eval(action2)));
                    };
                    var handleAction = function(v) {
                      if (v instanceof Initialize2) {
                        return bind(bindHalogenM)(get(monadStateHalogenM))(function(v1) {
                          return when$prime(applicativeHalogenM)(v1.formConfig.validateOnMount)(function(v2) {
                            return for_(applicativeHalogenM)(foldableArray)(getKeys(v1.fieldObject))(runValidation);
                          });
                        });
                      }
                      ;
                      if (v instanceof Receive2) {
                        return bind(bindHalogenM)(get(monadStateHalogenM))(function(v1) {
                          return when$prime(applicativeHalogenM)(!unsafeRefEq(v1.input)(v.value0))(function(v2) {
                            return modify_(monadStateHalogenM)(function(v3) {
                              var $135 = {};
                              for (var $136 in v3) {
                                if ({}.hasOwnProperty.call(v3, $136)) {
                                  $135[$136] = v3[$136];
                                }
                                ;
                              }
                              ;
                              $135.input = v.value0;
                              return $135;
                            });
                          });
                        });
                      }
                      ;
                      if (v instanceof HandleForm && v.value0 instanceof Raise2) {
                        return raise(v.value0.value0);
                      }
                      ;
                      if (v instanceof HandleForm && v.value0 instanceof Eval) {
                        if (v.value0.value0 instanceof SubmitForm) {
                          return discard(discardUnit)(bindHalogenM)(for_(applicativeHalogenM)(foldableMaybe)(v.value0.value0.value0)(function(event) {
                            return liftEffect(monadEffectHalogenM(dictMonadEffect))(preventDefault(event));
                          }))(function() {
                            return discard(discardUnit)(bindHalogenM)(bind(bindHalogenM)(get(monadStateHalogenM))(function(v1) {
                              return for_(applicativeHalogenM)(foldableArray)(getKeys(v1.fieldObject))(runValidation);
                            }))(function() {
                              return bind(bindHalogenM)(get(monadStateHalogenM))(function(v1) {
                                return discard(discardUnit)(bindHalogenM)(modify_(monadStateHalogenM)(function(state3) {
                                  var $149 = {};
                                  for (var $150 in state3) {
                                    if ({}.hasOwnProperty.call(state3, $150)) {
                                      $149[$150] = state3[$150];
                                    }
                                    ;
                                  }
                                  ;
                                  $149.formState = function() {
                                    var $146 = {};
                                    for (var $147 in state3.formState) {
                                      if ({}.hasOwnProperty.call(state3.formState, $147)) {
                                        $146[$147] = state3["formState"][$147];
                                      }
                                      ;
                                    }
                                    ;
                                    $146.submitCount = state3.formState.submitCount + 1 | 0;
                                    return $146;
                                  }();
                                  return $149;
                                }))(function() {
                                  return for_(applicativeHalogenM)(foldableMaybe)(mkFieldResults(dictMkFieldResults)(v1.fieldObject))(function(results) {
                                    var v2 = mkFieldOutputs(dictMkFieldOutputs)(results);
                                    if (v2 instanceof Nothing) {
                                      return tell2()({
                                        reflectSymbol: function() {
                                          return "inner";
                                        }
                                      })(ordUnit)($$Proxy.value)(unit)(SubmitAttempt.create(results));
                                    }
                                    ;
                                    if (v2 instanceof Just) {
                                      return tell2()({
                                        reflectSymbol: function() {
                                          return "inner";
                                        }
                                      })(ordUnit)($$Proxy.value)(unit)(Submit.create(v2.value0));
                                    }
                                    ;
                                    throw new Error("Failed pattern match at Formless (line 429, column 75 - line 431, column 64): " + [v2.constructor.name]);
                                  });
                                });
                              });
                            });
                          });
                        }
                        ;
                        if (v.value0.value0 instanceof SetForm) {
                          return modify_(monadStateHalogenM)(function(state3) {
                            var $156 = {};
                            for (var $157 in state3) {
                              if ({}.hasOwnProperty.call(state3, $157)) {
                                $156[$157] = state3[$157];
                              }
                              ;
                            }
                            ;
                            $156.fieldObject = v.value0.value0.value0;
                            return $156;
                          });
                        }
                        ;
                        if (v.value0.value0 instanceof SetFormConfig) {
                          return modify_(monadStateHalogenM)(function(state3) {
                            var $160 = {};
                            for (var $161 in state3) {
                              if ({}.hasOwnProperty.call(state3, $161)) {
                                $160[$161] = state3[$161];
                              }
                              ;
                            }
                            ;
                            $160.formConfig = v.value0.value0.value0;
                            return $160;
                          });
                        }
                        ;
                        if (v.value0.value0 instanceof ResetForm) {
                          var reset2 = function(field) {
                            var $164 = {};
                            for (var $165 in field) {
                              if ({}.hasOwnProperty.call(field, $165)) {
                                $164[$165] = field[$165];
                              }
                              ;
                            }
                            ;
                            $164.value = field.initialValue;
                            $164.result = Nothing.value;
                            return $164;
                          };
                          return discard(discardUnit)(bindHalogenM)(modify_(monadStateHalogenM)(function(state3) {
                            var $167 = {};
                            for (var $168 in state3) {
                              if ({}.hasOwnProperty.call(state3, $168)) {
                                $167[$168] = state3[$168];
                              }
                              ;
                            }
                            ;
                            $167.fieldObject = map(functorObject)(reset2)(state3.fieldObject);
                            $167.formState = {
                              submitCount: 0,
                              errorCount: 0,
                              allTouched: false
                            };
                            return $167;
                          }))(function() {
                            return tell2()({
                              reflectSymbol: function() {
                                return "inner";
                              }
                            })(ordUnit)($$Proxy.value)(unit)(Reset.create);
                          });
                        }
                        ;
                        if (v.value0.value0 instanceof ChangeField) {
                          return bind(bindHalogenM)(get(monadStateHalogenM))(function(v1) {
                            var modify7 = mkFieldRep(v.value0.value0.value0)(function(v2) {
                              return {
                                value: v.value0.value0.value1,
                                initialValue: v2.initialValue,
                                result: v2.result
                              };
                            });
                            return discard(discardUnit)(bindHalogenM)(modify_(monadStateHalogenM)(function(state3) {
                              var $171 = {};
                              for (var $172 in state3) {
                                if ({}.hasOwnProperty.call(state3, $172)) {
                                  $171[$172] = state3[$172];
                                }
                                ;
                              }
                              ;
                              $171.fieldObject = modifyField(modify7)(state3.fieldObject);
                              return $171;
                            }))(function() {
                              return when$prime(applicativeHalogenM)(v1.formConfig.validateOnChange)(function(v2) {
                                return runFormAction(new ValidateField(v.value0.value0.value0));
                              });
                            });
                          });
                        }
                        ;
                        if (v.value0.value0 instanceof BlurField) {
                          return bind(bindHalogenM)(get(monadStateHalogenM))(function(v1) {
                            return when$prime(applicativeHalogenM)(v1.formConfig.validateOnBlur)(function(v2) {
                              return runFormAction(new ValidateField(v.value0.value0.value0));
                            });
                          });
                        }
                        ;
                        if (v.value0.value0 instanceof ModifyField) {
                          return bind(bindHalogenM)(get(monadStateHalogenM))(function(v1) {
                            var modify7 = mkFieldRep(v.value0.value0.value0)(v.value0.value0.value1);
                            return discard(discardUnit)(bindHalogenM)(modify_(monadStateHalogenM)(function(state3) {
                              var $182 = {};
                              for (var $183 in state3) {
                                if ({}.hasOwnProperty.call(state3, $183)) {
                                  $182[$183] = state3[$183];
                                }
                                ;
                              }
                              ;
                              $182.fieldObject = modifyField(modify7)(state3.fieldObject);
                              return $182;
                            }))(function() {
                              return when$prime(applicativeHalogenM)(v1.formConfig.validateOnModify)(function(v2) {
                                return runFormAction(new ValidateField(v.value0.value0.value0));
                              });
                            });
                          });
                        }
                        ;
                        if (v.value0.value0 instanceof ValidateField) {
                          return runValidation(fieldsKey(v.value0.value0.value0));
                        }
                        ;
                        if (v.value0.value0 instanceof ResetField) {
                          var reset2 = function(field) {
                            return {
                              value: field.initialValue,
                              result: Nothing.value,
                              initialValue: field.initialValue
                            };
                          };
                          var modify6 = mkFieldRep(v.value0.value0.value0)(reset2);
                          return modify_(monadStateHalogenM)(function(state3) {
                            var $189 = {};
                            for (var $190 in state3) {
                              if ({}.hasOwnProperty.call(state3, $190)) {
                                $189[$190] = state3[$190];
                              }
                              ;
                            }
                            ;
                            $189.fieldObject = modifyField(modify6)(state3.fieldObject);
                            return $189;
                          });
                        }
                        ;
                        throw new Error("Failed pattern match at Formless (line 421, column 33 - line 473, column 89): " + [v.value0.value0.constructor.name]);
                      }
                      ;
                      throw new Error("Failed pattern match at Formless (line 407, column 18 - line 473, column 89): " + [v.constructor.name]);
                    };
                    return mkComponent({
                      initialState,
                      render: function(state3) {
                        var context = {
                          input: state3.input,
                          fields: state3.fieldObject,
                          actions: state3.fieldActions,
                          formState: state3.formState,
                          formActions: state3.formActions
                        };
                        return slot()({
                          reflectSymbol: function() {
                            return "inner";
                          }
                        })(ordUnit)($$Proxy.value)(unit)(component)(context)(HandleForm.create);
                      },
                      "eval": mkEval({
                        initialize: new Just(Initialize2.value),
                        receive: function($217) {
                          return Just.create(Receive2.create($217));
                        },
                        finalize: Nothing.value,
                        handleAction,
                        handleQuery: function() {
                          var $218 = query()({
                            reflectSymbol: function() {
                              return "inner";
                            }
                          })(ordUnit)($$Proxy.value)(unit);
                          return function($219) {
                            return $218(Query2.create($219));
                          };
                        }()
                      })
                    });
                  };
                };
              };
            };
          };
        };
      };
    };
  };
  var $$eval = function($220) {
    return raise(Eval.create($220));
  };
  var defaultConfig = {
    validateOnBlur: true,
    validateOnChange: false,
    validateOnModify: false,
    validateOnMount: false
  };
  var mkConfig1 = function(dictDefaults) {
    return {
      mkConfig: function(provided) {
        return defaults(dictDefaults)(defaultConfig)(provided);
      },
      Defaults0: function() {
        return dictDefaults;
      }
    };
  };

  // output/Control.Monad.Except/index.js
  var runExcept = /* @__PURE__ */ function() {
    var $0 = unwrap();
    return function($1) {
      return $0(runExceptT($1));
    };
  }();

  // output/Foreign.Index/foreign.js
  function unsafeReadPropImpl(f, s, key, value13) {
    return value13 == null ? f : s(value13[key]);
  }

  // output/Foreign.Index/index.js
  var unsafeReadProp = function(dictMonad) {
    return function(k) {
      return function(value13) {
        return unsafeReadPropImpl(fail(dictMonad)(new TypeMismatch("object", typeOf(value13))), pure(applicativeExceptT(dictMonad)), k, value13);
      };
    };
  };
  var readProp = function(dictMonad) {
    return unsafeReadProp(dictMonad);
  };

  // output/Web.File.FileList/foreign.js
  function _item(index4) {
    return function(fileList) {
      return fileList.item(index4);
    };
  }

  // output/Web.File.FileList/index.js
  var item = function(i2) {
    var $1 = _item(i2);
    return function($2) {
      return toMaybe($1($2));
    };
  };
  var items = function(dictUnfoldable) {
    return function(fl) {
      return unfoldr(dictUnfoldable)(function(i2) {
        return map(functorMaybe)(flip(Tuple.create)(i2 + 1 | 0))(item(i2)(fl));
      })(0);
    };
  };

  // output/Web.HTML.Event.EventTypes/index.js
  var input2 = "input";
  var domcontentloaded = "DOMContentLoaded";
  var change = "change";
  var blur2 = "blur";

  // output/Web.HTML.HTMLInputElement/foreign.js
  function _files(input3) {
    return function() {
      return input3.files;
    };
  }

  // output/Web.HTML.HTMLInputElement/index.js
  var fromEventTarget = /* @__PURE__ */ unsafeReadProtoTagged("HTMLInputElement");
  var files = /* @__PURE__ */ function() {
    var $4 = map(functorEffect)(toMaybe);
    return function($5) {
      return $4(_files($5));
    };
  }();

  // output/Halogen.HTML.Events/index.js
  var handler$prime = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return map(functorMaybe)(Action.create)(f(ev));
      });
    };
  };
  var handler2 = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return new Just(new Action(f(ev)));
      });
    };
  };
  var onChange = /* @__PURE__ */ handler2(change);
  var onFileUpload = function(dictUnfoldable) {
    return function(f) {
      return handler2(change)(function() {
        var $7 = maybe(none(dictUnfoldable))(items(dictUnfoldable));
        var $8 = composeKleisli(bindMaybe)(target)(composeKleisli(bindMaybe)(fromEventTarget)(function($10) {
          return unsafePerformEffect(files($10));
        }));
        return function($9) {
          return f($7($8($9)));
        };
      }());
    };
  };
  var onSubmit = /* @__PURE__ */ handler2("submit");
  var focusHandler = unsafeCoerce2;
  var onBlur = /* @__PURE__ */ function() {
    var $43 = handler2(blur2);
    return function($44) {
      return $43(focusHandler($44));
    };
  }();
  var addForeignPropHandler = function(key) {
    return function(prop3) {
      return function(reader) {
        return function(f) {
          var go2 = function(a2) {
            return composeKleisliFlipped(bindExceptT(monadIdentity))(reader)(readProp(monadIdentity)(prop3))(unsafeToForeign(a2));
          };
          return handler$prime(key)(composeKleisli(bindMaybe)(currentTarget)(function(e) {
            return either($$const(Nothing.value))(function($73) {
              return Just.create(f($73));
            })(runExcept(go2(e)));
          }));
        };
      };
    };
  };
  var onChecked = /* @__PURE__ */ addForeignPropHandler(change)("checked")(/* @__PURE__ */ readBoolean(monadIdentity));
  var onValueInput = /* @__PURE__ */ addForeignPropHandler(input2)("value")(/* @__PURE__ */ readString(monadIdentity));

  // output/Example.Basic/index.js
  var Receive3 = /* @__PURE__ */ function() {
    function Receive8(value0) {
      this.value0 = value0;
    }
    ;
    Receive8.create = function(value0) {
      return new Receive8(value0);
    };
    return Receive8;
  }();
  var Eval2 = /* @__PURE__ */ function() {
    function Eval7(value0) {
      this.value0 = value0;
    }
    ;
    Eval7.create = function(value0) {
      return new Eval7(value0);
    };
    return Eval7;
  }();
  var form2 = /* @__PURE__ */ function() {
    var render3 = function(v) {
      return form([onSubmit(v.formActions.handleSubmit)])([div_([label_([text("Name")]), input([type_(isPropInputType)(InputText.value), onValueInput(v.actions.name.handleChange), onBlur(v.actions.name.handleBlur), function() {
        if (v.fields.name.result instanceof Nothing) {
          return placeholder("Jack");
        }
        ;
        if (v.fields.name.result instanceof Just && v.fields.name.result.value0 instanceof Left) {
          return attr2("aria-invalid")("true");
        }
        ;
        if (v.fields.name.result instanceof Just && v.fields.name.result.value0 instanceof Right) {
          return attr2("aria-invalid")("false");
        }
        ;
        throw new Error("Failed pattern match at Example.Basic (line 69, column 17 - line 72, column 81): " + [v.fields.name.result.constructor.name]);
      }()]), function() {
        if (v.fields.name.result instanceof Just && v.fields.name.result.value0 instanceof Left) {
          return small_([text(v.fields.name.result.value0.value0)]);
        }
        ;
        return text("");
      }()]), div_([label_([text("Message")]), textarea([onValueInput(v.actions.message.handleChange), onBlur(v.actions.message.handleBlur)])]), button([type_(isPropButtonType)(ButtonSubmit.value)])([text("Submit")])]);
    };
    var handleQuery2 = function() {
      var validation2 = {
        name: function(v) {
          if (v === "") {
            return new Left("Required");
          }
          ;
          return new Right(v);
        },
        message: Right.create
      };
      return handleSubmitValidate(raise2)(validate()()()())(validation2);
    }();
    var handleAction = function(v) {
      if (v instanceof Receive3) {
        return put(monadStateHalogenM)(v.value0);
      }
      ;
      if (v instanceof Eval2) {
        return $$eval(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Example.Basic (line 42, column 18 - line 44, column 33): " + [v.constructor.name]);
    };
    return formless(monadEffectAff)(mkFieldStates1(hmapRecord()(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "message";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "name";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexNil)()())()())))(mkFieldActions1(hmapWithIndexRecord()(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "message";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "message";
      }
    })(refl)())(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "name";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "name";
      }
    })(refl)())(mapRecordWithIndexNil)()())()())))(mkFieldResults1(hfoldlRecordWithIndex()(foldlRecordCons({
      reflectSymbol: function() {
        return "message";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "message";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "name";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "name";
      }
    })(refl)()())(foldlRecordNil)))))(mkFieldOutputs1(hfoldlRecordWithIndex()(foldlRecordCons({
      reflectSymbol: function() {
        return "message";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "message";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "name";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "name";
      }
    })(refl)()())(foldlRecordNil)))))(mkConfig1(defaultsRecord()()))({
      liftAction: Eval2.create
    })(mempty(monoidRecord()(monoidRecordCons({
      reflectSymbol: function() {
        return "message";
      }
    })(monoidString)()(monoidRecordCons({
      reflectSymbol: function() {
        return "name";
      }
    })(monoidString)()(monoidRecordNil)))))(mkComponent({
      initialState: identity(categoryFn),
      render: render3,
      "eval": mkEval({
        handleAction,
        handleQuery: handleQuery2,
        receive: function($20) {
          return Just.create(Receive3.create($20));
        },
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize
      })
    }));
  }();

  // output/Example.Utils.Field/index.js
  var withLabel = function(v) {
    return function(html2) {
      return div_([label_([text(v.label)]), html2, function() {
        if (v.state.result instanceof Just && v.state.result.value0 instanceof Left) {
          return small_([text(v.state.result.value0.value0)]);
        }
        ;
        return text("");
      }()]);
    };
  };
  var textInput = function(v) {
    var $57 = withLabel({
      label: v.label,
      state: v.state
    });
    var $58 = append(semigroupArray)([value(isPropString)(v.state.value), function() {
      if (v.state.result instanceof Nothing) {
        return attr2("aria-touched")("false");
      }
      ;
      if (v.state.result instanceof Just && v.state.result.value0 instanceof Left) {
        return attr2("aria-invalid")("true");
      }
      ;
      if (v.state.result instanceof Just && v.state.result.value0 instanceof Right) {
        return attr2("aria-invalid")("false");
      }
      ;
      throw new Error("Failed pattern match at Example.Utils.Field (line 32, column 7 - line 35, column 71): " + [v.state.result.constructor.name]);
    }(), onValueInput(v.action.handleChange), onBlur(v.action.handleBlur)]);
    return function($59) {
      return $57(input($58($59)));
    };
  };
  var radioGroup = function(dictEq) {
    return function(v) {
      return div_([label_([text(v.label)]), fieldset_(mapFlipped(functorArray)(v.options)(function(v1) {
        return label_([input(flip(append(semigroupArray))(v1.props)([type_(isPropInputType)(InputRadio.value), name2(v.action.key), checked(eq(dictEq)(v.state.value)(v1.option)), onChange(function(v2) {
          return v.action.handleChange(v1.option);
        }), onBlur(v.action.handleBlur)])), text(v1.render)]);
      }))]);
    };
  };
  var fileUpload = function(v) {
    return function(props) {
      return div_([label([$$for(v.action.key)])([text(v.label), input(flip(append(semigroupArray))(props)([name2(v.action.key), type_(isPropInputType)(InputFile.value), onFileUpload(unfoldableArray)(v.action.handleChange), onBlur(v.action.handleBlur)]))]), function() {
        if (v.state.result instanceof Just && v.state.result.value0 instanceof Left) {
          return small_([text(v.state.result.value0.value0)]);
        }
        ;
        if (v.state.result instanceof Just && v.state.result.value0 instanceof Right) {
          return small_([v.onValid(v.state.result.value0.value0)]);
        }
        ;
        return text("");
      }()]);
    };
  };
  var fileUpload_ = /* @__PURE__ */ flip(fileUpload)([]);
  var checkbox = function(v) {
    return function(props) {
      return fieldset_([label_([input(flip(append(semigroupArray))(props)([type_(isPropInputType)(InputCheckbox.value), checked(v.state.value), onChecked(v.action.handleChange), onBlur(v.action.handleBlur)])), text(v.label)])]);
    };
  };
  var checkbox_ = /* @__PURE__ */ flip(checkbox)([]);

  // output/Example.Utils.Types/index.js
  var Username = function(x) {
    return x;
  };
  var One = /* @__PURE__ */ function() {
    function One2() {
    }
    ;
    One2.value = new One2();
    return One2;
  }();
  var Two2 = /* @__PURE__ */ function() {
    function Two3() {
    }
    ;
    Two3.value = new Two3();
    return Two3;
  }();
  var Three2 = /* @__PURE__ */ function() {
    function Three3() {
    }
    ;
    Three3.value = new Three3();
    return Three3;
  }();
  var showUsername = {
    show: function(v) {
      return "(Username " + (v + ")");
    }
  };
  var showPicked = {
    show: function(v) {
      if (v instanceof One) {
        return "One";
      }
      ;
      if (v instanceof Two2) {
        return "Two";
      }
      ;
      if (v instanceof Three2) {
        return "Three";
      }
      ;
      throw new Error("Failed pattern match at Example.Utils.Types (line 12, column 10 - line 15, column 21): " + [v.constructor.name]);
    }
  };
  var showEmail = {
    show: function(v) {
      return "(Email " + (v + ")");
    }
  };
  var eqPicked = {
    eq: function(x) {
      return function(y) {
        if (x instanceof One && y instanceof One) {
          return true;
        }
        ;
        if (x instanceof Two2 && y instanceof Two2) {
          return true;
        }
        ;
        if (x instanceof Three2 && y instanceof Three2) {
          return true;
        }
        ;
        return false;
      };
    }
  };

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _countPrefix = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasStringIterator) {
        return function(pred2) {
          return function(str) {
            var iter = str[Symbol.iterator]();
            for (var cpCount = 0; ; ++cpCount) {
              var o = iter.next();
              if (o.done)
                return cpCount;
              var cp = unsafeCodePointAt02(o.value);
              if (!pred2(cp))
                return cpCount;
            }
          };
        };
      }
      return fallback;
    };
  };
  var _singleton = function(fallback) {
    return hasFromCodePoint ? String.fromCodePoint : fallback;
  };
  var _take = function(fallback) {
    return function(n) {
      if (hasStringIterator) {
        return function(str) {
          var accum = "";
          var iter = str[Symbol.iterator]();
          for (var i2 = 0; i2 < n; ++i2) {
            var o = iter.next();
            if (o.done)
              return accum;
            accum += o.value;
          }
          return accum;
        };
      }
      return fallback(n);
    };
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.String.CodePoints/index.js
  var CodePoint = function(x) {
    return x;
  };
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons5 = function(s) {
    var v = length3(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum(boundedEnumChar)(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum(boundedEnumChar)(charAt(1)(s));
    var cu0 = fromEnum(boundedEnumChar)(charAt(0)(s));
    var $21 = isLead(cu0) && isTrail(cu1);
    if ($21) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop2(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop2(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map(functorMaybe)(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons5(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr(unfoldableArray)(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum(boundedEnumChar)(charAt(0)(s));
    var $25 = isLead(cu0) && length3(s) > 1;
    if ($25) {
      var cu1 = fromEnum(boundedEnumChar)(charAt(1)(s));
      var $26 = isTrail(cu1);
      if ($26) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var length6 = function($52) {
    return length(toCodePointArray($52));
  };
  var indexOf2 = function(p2) {
    return function(s) {
      return map(functorMaybe)(function(i2) {
        return length6(take2(i2)(s));
      })(indexOf(p2)(s));
    };
  };
  var fromCharCode2 = /* @__PURE__ */ function() {
    var $53 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($54) {
      return singleton6($53($54));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div(euclideanRingInt)(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod(euclideanRingInt)(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode2(lead) + fromCharCode2(trail);
  };
  var singleton7 = /* @__PURE__ */ _singleton(singletonFallback);
  var takeFallback = function(n) {
    return function(v) {
      if (n < 1) {
        return "";
      }
      ;
      var v1 = uncons5(v);
      if (v1 instanceof Just) {
        return singleton7(v1.value0.head) + takeFallback(n - 1 | 0)(v1.value0.tail);
      }
      ;
      return v;
    };
  };
  var take3 = /* @__PURE__ */ _take(takeFallback);
  var splitAt2 = function(i2) {
    return function(s) {
      var before = take3(i2)(s);
      return {
        before,
        after: drop2(length3(before))(s)
      };
    };
  };
  var eqCodePoint = {
    eq: function(x) {
      return function(y) {
        return x === y;
      };
    }
  };
  var drop4 = function(n) {
    return function(s) {
      return drop2(length3(take3(n)(s)))(s);
    };
  };
  var countTail = function($copy_p) {
    return function($copy_s) {
      return function($copy_accum) {
        var $tco_var_p = $copy_p;
        var $tco_var_s = $copy_s;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(p2, s, accum) {
          var v = uncons5(s);
          if (v instanceof Just) {
            var $39 = p2(v.value0.head);
            if ($39) {
              $tco_var_p = p2;
              $tco_var_s = v.value0.tail;
              $copy_accum = accum + 1 | 0;
              return;
            }
            ;
            $tco_done = true;
            return accum;
          }
          ;
          $tco_done = true;
          return accum;
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_p, $tco_var_s, $copy_accum);
        }
        ;
        return $tco_result;
      };
    };
  };
  var countFallback = function(p2) {
    return function(s) {
      return countTail(p2)(s)(0);
    };
  };
  var countPrefix2 = /* @__PURE__ */ _countPrefix(countFallback)(unsafeCodePointAt0);
  var takeWhile2 = function(p2) {
    return function(s) {
      return take3(countPrefix2(p2)(s))(s);
    };
  };
  var codePointFromChar = /* @__PURE__ */ function() {
    var $55 = fromEnum(boundedEnumChar);
    return function($56) {
      return CodePoint($55($56));
    };
  }();

  // output/Example.Utils.Validation/index.js
  var requiredText = function(input3) {
    if (input3 === "") {
      return new Left("Required.");
    }
    ;
    if (otherwise) {
      return new Right(input3);
    }
    ;
    throw new Error("Failed pattern match at Example.Utils.Validation (line 13, column 1 - line 13, column 47): " + [input3.constructor.name]);
  };
  var longerThan = function(limit) {
    return function(str) {
      if (length6(str) <= limit) {
        return new Left("Must be longer than " + (show(showInt)(limit) + " characters."));
      }
      ;
      if (otherwise) {
        return new Right(str);
      }
      ;
      throw new Error("Failed pattern match at Example.Utils.Validation (line 23, column 1 - line 23, column 52): " + [limit.constructor.name, str.constructor.name]);
    };
  };
  var username = /* @__PURE__ */ function() {
    var $6 = map(functorEither)(Username);
    var $7 = composeKleisliFlipped(bindEither)(longerThan(5))(requiredText);
    return function($8) {
      return $6($7($8));
    };
  }();
  var email = function(str) {
    if (!contains("@")(str)) {
      return new Left("Must contain the @ character.");
    }
    ;
    if (!contains(".")(str)) {
      return new Left("Must end in a top-level domain like '.com'.");
    }
    ;
    if (length6(str) <= 5) {
      return new Left("Not a valid email address.");
    }
    ;
    if (otherwise) {
      return new Right(str);
    }
    ;
    throw new Error("Failed pattern match at Example.Utils.Validation (line 28, column 1 - line 28, column 39): " + [str.constructor.name]);
  };

  // output/Example.CheckboxRadio/index.js
  var Receive4 = /* @__PURE__ */ function() {
    function Receive8(value0) {
      this.value0 = value0;
    }
    ;
    Receive8.create = function(value0) {
      return new Receive8(value0);
    };
    return Receive8;
  }();
  var Eval3 = /* @__PURE__ */ function() {
    function Eval7(value0) {
      this.value0 = value0;
    }
    ;
    Eval7.create = function(value0) {
      return new Eval7(value0);
    };
    return Eval7;
  }();
  var form3 = /* @__PURE__ */ function() {
    var render3 = function(v) {
      return form([onSubmit(v.formActions.handleSubmit)])([textInput({
        label: "Name",
        state: v.fields.name,
        action: v.actions.name
      })([placeholder("Jack")]), checkbox_({
        label: "Subscribe",
        state: v.fields.subscribe,
        action: v.actions.subscribe
      }), radioGroup(eqPicked)({
        label: "Pick One",
        options: [{
          option: One.value,
          render: "One",
          props: []
        }, {
          option: Two2.value,
          render: "Two",
          props: []
        }, {
          option: Three2.value,
          render: "Three",
          props: []
        }],
        state: v.fields.picked,
        action: v.actions.picked
      }), br_, button([type_(isPropButtonType)(ButtonSubmit.value)])([text("Submit")])]);
    };
    var initialForm = {
      name: "",
      subscribe: false,
      picked: One.value
    };
    var handleQuery2 = handleSubmitValidate(raise2)(validate()()()())({
      name: requiredText,
      subscribe: Right.create,
      picked: Right.create
    });
    var handleAction = function(v) {
      if (v instanceof Receive4) {
        return put(monadStateHalogenM)(v.value0);
      }
      ;
      if (v instanceof Eval3) {
        return $$eval(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Example.CheckboxRadio (line 46, column 18 - line 48, column 33): " + [v.constructor.name]);
    };
    return formless(monadEffectAff)(mkFieldStates1(hmapRecord()(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "name";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "picked";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "subscribe";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexNil)()())()())()())))(mkFieldActions1(hmapWithIndexRecord()(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "name";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "name";
      }
    })(refl)())(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "picked";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "picked";
      }
    })(refl)())(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "subscribe";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "subscribe";
      }
    })(refl)())(mapRecordWithIndexNil)()())()())()())))(mkFieldResults1(hfoldlRecordWithIndex()(foldlRecordCons({
      reflectSymbol: function() {
        return "name";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "name";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "picked";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "picked";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "subscribe";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "subscribe";
      }
    })(refl)()())(foldlRecordNil))))))(mkFieldOutputs1(hfoldlRecordWithIndex()(foldlRecordCons({
      reflectSymbol: function() {
        return "name";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "name";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "picked";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "picked";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "subscribe";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "subscribe";
      }
    })(refl)()())(foldlRecordNil))))))(mkConfig1(defaultsRecord()()))({
      liftAction: Eval3.create
    })(initialForm)(mkComponent({
      initialState: identity(categoryFn),
      render: render3,
      "eval": mkEval({
        handleAction,
        handleQuery: handleQuery2,
        receive: function($10) {
          return Just.create(Receive4.create($10));
        },
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize
      })
    }));
  }();

  // output/Example.DependentFields/index.js
  var Receive5 = /* @__PURE__ */ function() {
    function Receive8(value0) {
      this.value0 = value0;
    }
    ;
    Receive8.create = function(value0) {
      return new Receive8(value0);
    };
    return Receive8;
  }();
  var Eval4 = /* @__PURE__ */ function() {
    function Eval7(value0) {
      this.value0 = value0;
    }
    ;
    Eval7.create = function(value0) {
      return new Eval7(value0);
    };
    return Eval7;
  }();
  var form4 = /* @__PURE__ */ function() {
    var render3 = function(v) {
      return form([onSubmit(v.formActions.handleSubmit)])([textInput({
        label: "Email",
        state: v.fields.email,
        action: v.actions.email
      })([placeholder("jack@kerouac.com")]), textInput({
        label: "Username",
        state: v.fields.username,
        action: v.actions.username
      })([placeholder("jk")]), textInput({
        label: "Password",
        state: v.fields.password1,
        action: v.actions.password1
      })([type_(isPropInputType)(InputPassword.value)]), textInput({
        label: "Password (Confirm)",
        state: v.fields.password2,
        action: v.actions.password2
      })([type_(isPropInputType)(InputPassword.value)]), button([type_(isPropButtonType)(ButtonSubmit.value)])([text("Submit")])]);
    };
    var handleAction = function(v) {
      if (v instanceof Receive5) {
        return put(monadStateHalogenM)(v.value0);
      }
      ;
      if (v instanceof Eval4) {
        return $$eval(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Example.DependentFields (line 55, column 18 - line 57, column 33): " + [v.constructor.name]);
    };
    var handleQuery2 = function() {
      var validatePasswords = function(input3) {
        return function(otherPassword) {
          return runExceptT(function() {
            var validateEq = function(a2) {
              return function(b2) {
                if (a2 === b2) {
                  return new Right(a2);
                }
                ;
                if (otherwise) {
                  return new Left("Passwords must match.");
                }
                ;
                throw new Error("Failed pattern match at Example.DependentFields (line 68, column 11 - line 70, column 55): " + [a2.constructor.name, b2.constructor.name]);
              };
            };
            return bind(bindExceptT(monadHalogenM))(except(applicativeHalogenM)(bindFlipped(bindEither)(longerThan(3))(requiredText(input3))))(function(password) {
              return bind(bindExceptT(monadHalogenM))(except(applicativeHalogenM)(validateEq(password)(otherPassword.value)))(function() {
                return discard(discardUnit)(bindExceptT(monadHalogenM))(function() {
                  if (otherPassword.result instanceof Just && otherPassword.result.value0 instanceof Left) {
                    return lift(monadTransExceptT)(monadHalogenM)($$void(functorHalogenM)(fork(discard(discardUnit)(bindHalogenM)(liftAff(monadAffHalogenM(monadAffAff))(delay(10)))(function() {
                      return handleAction(otherPassword.validate);
                    }))));
                  }
                  ;
                  return pure(applicativeExceptT(monadHalogenM))(unit);
                }())(function() {
                  return pure(applicativeExceptT(monadHalogenM))(password);
                });
              });
            });
          }());
        };
      };
      var validation2 = {
        email: function(input3) {
          var validated = email(input3);
          return bind(bindHalogenM)(gets(monadStateHalogenM)(function(v) {
            return v.fields.username.result;
          }))(function(usernameResult) {
            return discard(discardUnit)(bindHalogenM)(when(applicativeHalogenM)(isRight(validated) && isNothing(usernameResult))(function() {
              var start2 = takeWhile2(function(v) {
                return notEq(eqCodePoint)(v)(codePointFromChar("@"));
              })(input3);
              return bind(bindHalogenM)(gets(monadStateHalogenM)(function(v) {
                return v.actions.username.modify;
              }))(function(modifyUsername) {
                return handleAction(modifyUsername(function(v) {
                  var $28 = {};
                  for (var $29 in v) {
                    if ({}.hasOwnProperty.call(v, $29)) {
                      $28[$29] = v[$29];
                    }
                    ;
                  }
                  ;
                  $28.value = start2;
                  return $28;
                }));
              });
            }()))(function() {
              return pure(applicativeHalogenM)(validated);
            });
          });
        },
        username: function() {
          var $45 = pure(applicativeHalogenM);
          return function($46) {
            return $45(username($46));
          };
        }(),
        password1: function(input3) {
          return bind(bindHalogenM)(gets(monadStateHalogenM)(function(v) {
            return v.fields.password2;
          }))(function(v) {
            return bind(bindHalogenM)(gets(monadStateHalogenM)(function(v1) {
              return v1.actions.password2;
            }))(function(v1) {
              return validatePasswords(input3)({
                value: v.value,
                result: v.result,
                validate: v1.validate
              });
            });
          });
        },
        password2: function(input3) {
          return bind(bindHalogenM)(gets(monadStateHalogenM)(function(v) {
            return v.fields.password1;
          }))(function(v) {
            return bind(bindHalogenM)(gets(monadStateHalogenM)(function(v1) {
              return v1.actions.password1;
            }))(function(v1) {
              return validatePasswords(input3)({
                value: v.value,
                result: v.result,
                validate: v1.validate
              });
            });
          });
        }
      };
      var toOutput = function(v) {
        return {
          email: v.email,
          username: v.username,
          password: v.password1
        };
      };
      return handleSubmitValidateM(function($47) {
        return raise2(toOutput($47));
      })(validateM()()()()(applicativeHalogenM))(validation2);
    }();
    return formless(monadEffectAff)(mkFieldStates1(hmapRecord()(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "email";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "password1";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "password2";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "username";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexNil)()())()())()())()())))(mkFieldActions1(hmapWithIndexRecord()(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "email";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "email";
      }
    })(refl)())(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "password1";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "password1";
      }
    })(refl)())(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "password2";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "password2";
      }
    })(refl)())(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "username";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "username";
      }
    })(refl)())(mapRecordWithIndexNil)()())()())()())()())))(mkFieldResults1(hfoldlRecordWithIndex()(foldlRecordCons({
      reflectSymbol: function() {
        return "email";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "email";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "password1";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "password1";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "password2";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "password2";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "username";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "username";
      }
    })(refl)()())(foldlRecordNil)))))))(mkFieldOutputs1(hfoldlRecordWithIndex()(foldlRecordCons({
      reflectSymbol: function() {
        return "email";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "email";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "password1";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "password1";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "password2";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "password2";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "username";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "username";
      }
    })(refl)()())(foldlRecordNil)))))))(mkConfig1(defaultsRecord()()))({
      liftAction: Eval4.create,
      validateOnModify: true
    })(mempty(monoidRecord()(monoidRecordCons({
      reflectSymbol: function() {
        return "email";
      }
    })(monoidString)()(monoidRecordCons({
      reflectSymbol: function() {
        return "password1";
      }
    })(monoidString)()(monoidRecordCons({
      reflectSymbol: function() {
        return "password2";
      }
    })(monoidString)()(monoidRecordCons({
      reflectSymbol: function() {
        return "username";
      }
    })(monoidString)()(monoidRecordNil)))))))(mkComponent({
      initialState: identity(categoryFn),
      render: render3,
      "eval": mkEval({
        handleAction,
        handleQuery: handleQuery2,
        receive: function($48) {
          return Just.create(Receive5.create($48));
        },
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize
      })
    }));
  }();

  // output/Data.MediaType.Common/index.js
  var imagePNG = "image/png";
  var imageJPEG = "image/jpeg";
  var imageGIF = "image/gif";

  // output/Web.File.File/foreign.js
  function name4(file) {
    return file.name;
  }

  // output/Web.File.Blob/foreign.js
  function typeImpl(blob) {
    return blob.type;
  }
  function size3(blob) {
    return blob.size;
  }

  // output/Web.File.Blob/index.js
  var type_4 = function(blob) {
    var blobType = typeImpl(blob);
    var $0 = blobType === "";
    if ($0) {
      return Nothing.value;
    }
    ;
    return new Just(blobType);
  };

  // output/Web.File.File/index.js
  var type_5 = function($0) {
    return type_4($0);
  };
  var size4 = function($1) {
    return size3($1);
  };

  // output/Example.FileUpload/index.js
  var Receive6 = /* @__PURE__ */ function() {
    function Receive8(value0) {
      this.value0 = value0;
    }
    ;
    Receive8.create = function(value0) {
      return new Receive8(value0);
    };
    return Receive8;
  }();
  var Eval5 = /* @__PURE__ */ function() {
    function Eval7(value0) {
      this.value0 = value0;
    }
    ;
    Eval7.create = function(value0) {
      return new Eval7(value0);
    };
    return Eval7;
  }();
  var validateFileType = function(file) {
    var v = type_5(file);
    if (v instanceof Nothing) {
      return new Left("Unrecognized file type. Accepted types: png, jpeg, gif.");
    }
    ;
    if (v instanceof Just && elem2(eqMediaType)(v.value0)([imagePNG, imageJPEG, imageGIF])) {
      return new Right(file);
    }
    ;
    if (v instanceof Just) {
      return new Left("Unsupported file type: " + (unwrap()(v.value0) + ". Accepted types: png, jpeg, gif."));
    }
    ;
    throw new Error("Failed pattern match at Example.FileUpload (line 86, column 25 - line 89, column 98): " + [v.constructor.name]);
  };
  var validateFileSize = function(file) {
    var v = size4(file);
    if (v > 99999) {
      return new Left("Photos must be smaller than 100kb.");
    }
    ;
    if (v < 1e3) {
      return new Left("Photos cannot be smaller than 1kb.");
    }
    ;
    if (otherwise) {
      return new Right({
        name: name4(file),
        size: v
      });
    }
    ;
    throw new Error("Failed pattern match at Example.FileUpload (line 92, column 25 - line 96, column 56): " + [v.constructor.name]);
  };
  var validateFileCount = function(files2) {
    var v = uncons(files2);
    if (v instanceof Just && v.value0.tail.length === 0) {
      return new Right(v.value0.head);
    }
    ;
    if (v instanceof Just) {
      return new Left("Only one photo can be uploaded.");
    }
    ;
    if (v instanceof Nothing) {
      return new Left("Required.");
    }
    ;
    throw new Error("Failed pattern match at Example.FileUpload (line 80, column 27 - line 83, column 30): " + [v.constructor.name]);
  };
  var form5 = /* @__PURE__ */ function() {
    var render3 = function(v) {
      return form([onSubmit(v.formActions.handleSubmit)])([textInput({
        label: "Name",
        state: v.fields.name,
        action: v.actions.name
      })([placeholder("Jack")]), fileUpload_({
        label: "Upload Photo",
        state: v.fields.photo,
        action: v.actions.photo,
        onValid: function(v1) {
          return small_([text("Your photo size: " + show(showNumber)(v1.size))]);
        }
      }), br_, button([type_(isPropButtonType)(ButtonSubmit.value)])([text("Submit")])]);
    };
    var handleQuery2 = handleSubmitValidate(raise2)(validate()()()())({
      name: requiredText,
      photo: composeKleisliFlipped(bindEither)(validateFileSize)(composeKleisliFlipped(bindEither)(validateFileType)(validateFileCount))
    });
    var handleAction = function(v) {
      if (v instanceof Receive6) {
        return put(monadStateHalogenM)(v.value0);
      }
      ;
      if (v instanceof Eval5) {
        return $$eval(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Example.FileUpload (line 47, column 18 - line 49, column 33): " + [v.constructor.name]);
    };
    return formless(monadEffectAff)(mkFieldStates1(hmapRecord()(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "name";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "photo";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexNil)()())()())))(mkFieldActions1(hmapWithIndexRecord()(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "name";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "name";
      }
    })(refl)())(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "photo";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "photo";
      }
    })(refl)())(mapRecordWithIndexNil)()())()())))(mkFieldResults1(hfoldlRecordWithIndex()(foldlRecordCons({
      reflectSymbol: function() {
        return "name";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "name";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "photo";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "photo";
      }
    })(refl)()())(foldlRecordNil)))))(mkFieldOutputs1(hfoldlRecordWithIndex()(foldlRecordCons({
      reflectSymbol: function() {
        return "name";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "name";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "photo";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "photo";
      }
    })(refl)()())(foldlRecordNil)))))(mkConfig1(defaultsRecord()()))({
      liftAction: Eval5.create
    })(mempty(monoidRecord()(monoidRecordCons({
      reflectSymbol: function() {
        return "name";
      }
    })(monoidString)()(monoidRecordCons({
      reflectSymbol: function() {
        return "photo";
      }
    })(monoidArray)()(monoidRecordNil)))))(mkComponent({
      initialState: identity(categoryFn),
      render: render3,
      "eval": mkEval({
        handleAction,
        handleQuery: handleQuery2,
        receive: function($25) {
          return Just.create(Receive6.create($25));
        },
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize
      })
    }));
  }();

  // output/Data.Argonaut.Core/foreign.js
  function id2(x) {
    return x;
  }
  var jsonNull = null;
  function stringify(j) {
    return JSON.stringify(j);
  }
  function _caseJson(isNull3, isBool, isNum, isStr, isArr, isObj, j) {
    if (j == null)
      return isNull3();
    else if (typeof j === "boolean")
      return isBool(j);
    else if (typeof j === "number")
      return isNum(j);
    else if (typeof j === "string")
      return isStr(j);
    else if (Object.prototype.toString.call(j) === "[object Array]")
      return isArr(j);
    else
      return isObj(j);
  }

  // output/Data.Argonaut.Core/index.js
  var verbJsonType = function(def) {
    return function(f) {
      return function(g) {
        return g(def)(f);
      };
    };
  };
  var toJsonType = /* @__PURE__ */ function() {
    return verbJsonType(Nothing.value)(Just.create);
  }();
  var isJsonType = /* @__PURE__ */ verbJsonType(false)(/* @__PURE__ */ $$const(true));
  var caseJsonString = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), $$const(d), $$const(d), f, $$const(d), $$const(d), j);
      };
    };
  };
  var toString = /* @__PURE__ */ toJsonType(caseJsonString);
  var caseJsonObject = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), $$const(d), $$const(d), $$const(d), $$const(d), f, j);
      };
    };
  };
  var toObject = /* @__PURE__ */ toJsonType(caseJsonObject);
  var caseJsonNull = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson(f, $$const(d), $$const(d), $$const(d), $$const(d), $$const(d), j);
      };
    };
  };
  var isNull2 = /* @__PURE__ */ isJsonType(caseJsonNull);

  // output/Data.Argonaut.Decode.Error/index.js
  var TypeMismatch2 = /* @__PURE__ */ function() {
    function TypeMismatch3(value0) {
      this.value0 = value0;
    }
    ;
    TypeMismatch3.create = function(value0) {
      return new TypeMismatch3(value0);
    };
    return TypeMismatch3;
  }();
  var UnexpectedValue = /* @__PURE__ */ function() {
    function UnexpectedValue2(value0) {
      this.value0 = value0;
    }
    ;
    UnexpectedValue2.create = function(value0) {
      return new UnexpectedValue2(value0);
    };
    return UnexpectedValue2;
  }();
  var AtIndex = /* @__PURE__ */ function() {
    function AtIndex2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AtIndex2.create = function(value0) {
      return function(value1) {
        return new AtIndex2(value0, value1);
      };
    };
    return AtIndex2;
  }();
  var AtKey = /* @__PURE__ */ function() {
    function AtKey2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AtKey2.create = function(value0) {
      return function(value1) {
        return new AtKey2(value0, value1);
      };
    };
    return AtKey2;
  }();
  var Named = /* @__PURE__ */ function() {
    function Named2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Named2.create = function(value0) {
      return function(value1) {
        return new Named2(value0, value1);
      };
    };
    return Named2;
  }();
  var MissingValue = /* @__PURE__ */ function() {
    function MissingValue2() {
    }
    ;
    MissingValue2.value = new MissingValue2();
    return MissingValue2;
  }();
  var printJsonDecodeError = function(err) {
    var go2 = function(v) {
      if (v instanceof TypeMismatch2) {
        return "  Expected value of type '" + (v.value0 + "'.");
      }
      ;
      if (v instanceof UnexpectedValue) {
        return "  Unexpected value " + (stringify(v.value0) + ".");
      }
      ;
      if (v instanceof AtIndex) {
        return "  At array index " + (show(showInt)(v.value0) + (":\n" + go2(v.value1)));
      }
      ;
      if (v instanceof AtKey) {
        return "  At object key '" + (v.value0 + ("':\n" + go2(v.value1)));
      }
      ;
      if (v instanceof Named) {
        return "  Under '" + (v.value0 + ("':\n" + go2(v.value1)));
      }
      ;
      if (v instanceof MissingValue) {
        return "  No value was found.";
      }
      ;
      throw new Error("Failed pattern match at Data.Argonaut.Decode.Error (line 37, column 8 - line 43, column 44): " + [v.constructor.name]);
    };
    return "An error occurred while decoding a JSON value:\n" + go2(err);
  };

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head4, tail2) {
      this.head = head4;
      this.tail = tail2;
    };
    function finalCell(head4) {
      return new ConsCell(head4, emptyList);
    }
    function consList(x) {
      return function(xs) {
        return new ConsCell(x, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply2) {
      return function(map3) {
        return function(f) {
          var buildFrom = function(x, ys) {
            return apply2(map3(consList)(f(x)))(ys);
          };
          var go2 = function(acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last3 = xs[currentLen - 1];
              return new Cont(function() {
                var built = go2(buildFrom(last3, acc), currentLen - 1, xs);
                return built;
              });
            }
          };
          return function(array) {
            var acc = map3(finalCell)(f(array[array.length - 1]));
            var result = go2(acc, array.length - 1, array);
            while (result instanceof Cont) {
              result = result.fn();
            }
            return map3(listToArray)(result);
          };
        };
      };
    };
  }();

  // output/Data.Argonaut.Decode.Decoders/index.js
  var decodeString = /* @__PURE__ */ function() {
    return caseJsonString(new Left(new TypeMismatch2("String")))(Right.create);
  }();
  var decodeMaybe = function(decoder) {
    return function(json) {
      if (isNull2(json)) {
        return pure(applicativeEither)(Nothing.value);
      }
      ;
      if (otherwise) {
        return map(functorEither)(Just.create)(decoder(json));
      }
      ;
      throw new Error("Failed pattern match at Data.Argonaut.Decode.Decoders (line 37, column 1 - line 41, column 38): " + [decoder.constructor.name, json.constructor.name]);
    };
  };
  var decodeJObject = /* @__PURE__ */ function() {
    var $20 = note(new TypeMismatch2("Object"));
    return function($21) {
      return $20(toObject($21));
    };
  }();
  var decodeEither = function(decoderA) {
    return function(decoderB) {
      return function(json) {
        return lmap(bifunctorEither)(Named.create("Either"))(bind(bindEither)(decodeJObject(json))(function(obj) {
          return bind(bindEither)(note(new AtKey("tag", MissingValue.value))(lookup2("tag")(obj)))(function(tag) {
            return bind(bindEither)(note(new AtKey("value", MissingValue.value))(lookup2("value")(obj)))(function(val) {
              var v = toString(tag);
              if (v instanceof Just && v.value0 === "Right") {
                return map(functorEither)(Right.create)(decoderB(val));
              }
              ;
              if (v instanceof Just && v.value0 === "Left") {
                return map(functorEither)(Left.create)(decoderA(val));
              }
              ;
              return new Left(new AtKey("tag", new UnexpectedValue(tag)));
            });
          });
        }));
      };
    };
  };

  // output/Data.Argonaut.Decode.Class/index.js
  var gDecodeJsonNil = {
    gDecodeJson: function(v) {
      return function(v1) {
        return new Right({});
      };
    }
  };
  var gDecodeJson = function(dict) {
    return dict.gDecodeJson;
  };
  var decodeRecord = function(dictGDecodeJson) {
    return function() {
      return {
        decodeJson: function(json) {
          var v = toObject(json);
          if (v instanceof Just) {
            return gDecodeJson(dictGDecodeJson)(v.value0)($$Proxy.value);
          }
          ;
          if (v instanceof Nothing) {
            return new Left(new TypeMismatch2("Object"));
          }
          ;
          throw new Error("Failed pattern match at Data.Argonaut.Decode.Class (line 103, column 5 - line 105, column 46): " + [v.constructor.name]);
        }
      };
    };
  };
  var decodeJsonString = {
    decodeJson: decodeString
  };
  var decodeJsonField = function(dict) {
    return dict.decodeJsonField;
  };
  var gDecodeJsonCons = function(dictDecodeJsonField) {
    return function(dictGDecodeJson) {
      return function(dictIsSymbol) {
        return function() {
          return function() {
            return {
              gDecodeJson: function(object2) {
                return function(v) {
                  var fieldName = reflectSymbol(dictIsSymbol)($$Proxy.value);
                  var fieldValue = lookup2(fieldName)(object2);
                  var v1 = decodeJsonField(dictDecodeJsonField)(fieldValue);
                  if (v1 instanceof Just) {
                    return bind(bindEither)(lmap(bifunctorEither)(AtKey.create(fieldName))(v1.value0))(function(val) {
                      return bind(bindEither)(gDecodeJson(dictGDecodeJson)(object2)($$Proxy.value))(function(rest) {
                        return new Right(insert(dictIsSymbol)()()($$Proxy.value)(val)(rest));
                      });
                    });
                  }
                  ;
                  if (v1 instanceof Nothing) {
                    return new Left(new AtKey(fieldName, MissingValue.value));
                  }
                  ;
                  throw new Error("Failed pattern match at Data.Argonaut.Decode.Class (line 127, column 5 - line 134, column 44): " + [v1.constructor.name]);
                };
              }
            };
          };
        };
      };
    };
  };
  var decodeJson = function(dict) {
    return dict.decodeJson;
  };
  var decodeJsonEither = function(dictDecodeJson) {
    return function(dictDecodeJson1) {
      return {
        decodeJson: decodeEither(decodeJson(dictDecodeJson))(decodeJson(dictDecodeJson1))
      };
    };
  };
  var decodeJsonMaybe = function(dictDecodeJson) {
    return {
      decodeJson: decodeMaybe(decodeJson(dictDecodeJson))
    };
  };
  var decodeFieldMaybe = function(dictDecodeJson) {
    return {
      decodeJsonField: function(v) {
        if (v instanceof Nothing) {
          return new Just(new Right(Nothing.value));
        }
        ;
        if (v instanceof Just) {
          return new Just(decodeJson(decodeJsonMaybe(dictDecodeJson))(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Argonaut.Decode.Class (line 139, column 1 - line 143, column 49): " + [v.constructor.name]);
      }
    };
  };
  var decodeFieldId = function(dictDecodeJson) {
    return {
      decodeJsonField: function(j) {
        return map(functorMaybe)(decodeJson(dictDecodeJson))(j);
      }
    };
  };

  // output/Data.Argonaut.Parser/foreign.js
  function _jsonParser(fail2, succ2, s) {
    try {
      return succ2(JSON.parse(s));
    } catch (e) {
      return fail2(e.message);
    }
  }

  // output/Data.Argonaut.Parser/index.js
  var jsonParser = function(j) {
    return _jsonParser(Left.create, Right.create, j);
  };

  // output/Data.Argonaut.Decode.Parser/index.js
  var parseJson = /* @__PURE__ */ function() {
    var $1 = lmap(bifunctorEither)(function(v) {
      return new TypeMismatch2("JSON");
    });
    return function($2) {
      return $1(jsonParser($2));
    };
  }();

  // output/Data.Argonaut.Encode.Encoders/index.js
  var encodeString = id2;
  var encodeMaybe = function(encoder) {
    return function(v) {
      if (v instanceof Nothing) {
        return jsonNull;
      }
      ;
      if (v instanceof Just) {
        return encoder(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Data.Argonaut.Encode.Encoders (line 31, column 23 - line 33, column 22): " + [v.constructor.name]);
    };
  };
  var encodeEither = function(encoderA) {
    return function(encoderB) {
      var obj = function(encoder) {
        return function(tag) {
          return function(x) {
            return id2(fromFoldable2(foldableList)(new Cons(new Tuple("tag", id2(tag)), new Cons(new Tuple("value", encoder(x)), Nil.value))));
          };
        };
      };
      return either(obj(encoderA)("Left"))(obj(encoderB)("Right"));
    };
  };

  // output/Data.Argonaut.Encode.Class/index.js
  var gEncodeJsonNil = {
    gEncodeJson: function(v) {
      return function(v1) {
        return empty2;
      };
    }
  };
  var gEncodeJson = function(dict) {
    return dict.gEncodeJson;
  };
  var encodeRecord = function(dictGEncodeJson) {
    return function() {
      return {
        encodeJson: function(rec) {
          return id2(gEncodeJson(dictGEncodeJson)(rec)($$Proxy.value));
        }
      };
    };
  };
  var encodeJsonJString = {
    encodeJson: encodeString
  };
  var encodeJson = function(dict) {
    return dict.encodeJson;
  };
  var encodeJsonEither = function(dictEncodeJson) {
    return function(dictEncodeJson1) {
      return {
        encodeJson: encodeEither(encodeJson(dictEncodeJson))(encodeJson(dictEncodeJson1))
      };
    };
  };
  var encodeJsonMaybe = function(dictEncodeJson) {
    return {
      encodeJson: encodeMaybe(encodeJson(dictEncodeJson))
    };
  };
  var gEncodeJsonCons = function(dictEncodeJson) {
    return function(dictGEncodeJson) {
      return function(dictIsSymbol) {
        return function() {
          return {
            gEncodeJson: function(row) {
              return function(v) {
                return insert3(reflectSymbol(dictIsSymbol)($$Proxy.value))(encodeJson(dictEncodeJson)(get2(dictIsSymbol)()($$Proxy.value)(row)))(gEncodeJson(dictGEncodeJson)(row)($$Proxy.value));
              };
            }
          };
        };
      };
    };
  };

  // output/Effect.Console/foreign.js
  var log2 = function(s) {
    return function() {
      console.log(s);
    };
  };
  var warn = function(s) {
    return function() {
      console.warn(s);
    };
  };

  // output/Effect.Class.Console/index.js
  var log3 = function(dictMonadEffect) {
    var $33 = liftEffect(dictMonadEffect);
    return function($34) {
      return $33(log2($34));
    };
  };

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.HTMLDocument/foreign.js
  function _readyState(doc) {
    return function() {
      return doc.readyState;
    };
  }

  // output/Web.HTML.HTMLDocument.ReadyState/index.js
  var Loading = /* @__PURE__ */ function() {
    function Loading2() {
    }
    ;
    Loading2.value = new Loading2();
    return Loading2;
  }();
  var Interactive = /* @__PURE__ */ function() {
    function Interactive2() {
    }
    ;
    Interactive2.value = new Interactive2();
    return Interactive2;
  }();
  var Complete = /* @__PURE__ */ function() {
    function Complete2() {
    }
    ;
    Complete2.value = new Complete2();
    return Complete2;
  }();
  var parse = function(v) {
    if (v === "loading") {
      return new Just(Loading.value);
    }
    ;
    if (v === "interactive") {
      return new Just(Interactive.value);
    }
    ;
    if (v === "complete") {
      return new Just(Complete.value);
    }
    ;
    return Nothing.value;
  };

  // output/Web.HTML.HTMLDocument/index.js
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = /* @__PURE__ */ function() {
    var $0 = map(functorEffect)(function() {
      var $2 = fromMaybe(Loading.value);
      return function($3) {
        return $2(parse($3));
      };
    }());
    return function($1) {
      return $0(_readyState($1));
    };
  }();

  // output/Web.HTML.Location/foreign.js
  function hash(location2) {
    return function() {
      return location2.hash;
    };
  }

  // output/Web.HTML.Window/foreign.js
  function document(window2) {
    return function() {
      return window2.document;
    };
  }
  function location(window2) {
    return function() {
      return window2.location;
    };
  }
  function localStorage(window2) {
    return function() {
      return window2.localStorage;
    };
  }

  // output/Web.HTML.Window/index.js
  var toEventTarget = unsafeCoerce2;

  // output/Web.Storage.Storage/foreign.js
  function _getItem(key) {
    return function(storage) {
      return function() {
        return storage.getItem(key);
      };
    };
  }
  function setItem(key) {
    return function(value13) {
      return function(storage) {
        return function() {
          storage.setItem(key, value13);
        };
      };
    };
  }

  // output/Web.Storage.Storage/index.js
  var getItem = function(s) {
    var $3 = map(functorEffect)(toMaybe);
    var $4 = _getItem(s);
    return function($5) {
      return $3($4($5));
    };
  };

  // output/Example.LocalStorage/index.js
  var Initialize3 = /* @__PURE__ */ function() {
    function Initialize4() {
    }
    ;
    Initialize4.value = new Initialize4();
    return Initialize4;
  }();
  var Receive7 = /* @__PURE__ */ function() {
    function Receive8(value0) {
      this.value0 = value0;
    }
    ;
    Receive8.create = function(value0) {
      return new Receive8(value0);
    };
    return Receive8;
  }();
  var Eval6 = /* @__PURE__ */ function() {
    function Eval7(value0) {
      this.value0 = value0;
    }
    ;
    Eval7.create = function(value0) {
      return new Eval7(value0);
    };
    return Eval7;
  }();
  var form6 = /* @__PURE__ */ function() {
    var render3 = function(v) {
      return form([onSubmit(v.formActions.handleSubmit)])([textInput({
        label: "Name",
        state: v.fields.name,
        action: v.actions.name
      })([placeholder("Jack")]), textInput({
        label: "Nickname",
        state: v.fields.nickname,
        action: v.actions.nickname
      })([]), textInput({
        label: "City",
        state: v.fields.city,
        action: v.actions.city
      })([placeholder("Los Angeles")]), textInput({
        label: "State",
        state: v.fields.state,
        action: v.actions.state
      })([placeholder("California")]), br_, button([type_(isPropButtonType)(ButtonSubmit.value)])([text("Submit")])]);
    };
    var handleQuery2 = handleSubmitValidate(raise2)(validate()()()())({
      name: requiredText,
      nickname: Right.create,
      city: requiredText,
      state: requiredText
    });
    var handleAction = function(v) {
      if (v instanceof Initialize3) {
        return bind(bindHalogenM)(liftEffect(monadEffectHalogenM(monadEffectAff))(bindFlipped(bindEffect)(getItem("local-storage-form"))(bindFlipped(bindEffect)(localStorage)(windowImpl))))(function(storedState) {
          var v1 = bindFlipped(bindEither)(decodeJson(decodeRecord(gDecodeJsonCons(decodeFieldId(decodeRecord(gDecodeJsonCons(decodeFieldId(decodeJsonString))(gDecodeJsonCons(decodeFieldMaybe(decodeJsonEither(decodeJsonString)(decodeJsonString)))(gDecodeJsonCons(decodeFieldId(decodeJsonString))(gDecodeJsonNil)({
            reflectSymbol: function() {
              return "value";
            }
          })()())({
            reflectSymbol: function() {
              return "result";
            }
          })()())({
            reflectSymbol: function() {
              return "initialValue";
            }
          })()())()))(gDecodeJsonCons(decodeFieldId(decodeRecord(gDecodeJsonCons(decodeFieldId(decodeJsonString))(gDecodeJsonCons(decodeFieldMaybe(decodeJsonEither(decodeJsonString)(decodeJsonString)))(gDecodeJsonCons(decodeFieldId(decodeJsonString))(gDecodeJsonNil)({
            reflectSymbol: function() {
              return "value";
            }
          })()())({
            reflectSymbol: function() {
              return "result";
            }
          })()())({
            reflectSymbol: function() {
              return "initialValue";
            }
          })()())()))(gDecodeJsonCons(decodeFieldId(decodeRecord(gDecodeJsonCons(decodeFieldId(decodeJsonString))(gDecodeJsonCons(decodeFieldMaybe(decodeJsonEither(decodeJsonString)(decodeJsonString)))(gDecodeJsonCons(decodeFieldId(decodeJsonString))(gDecodeJsonNil)({
            reflectSymbol: function() {
              return "value";
            }
          })()())({
            reflectSymbol: function() {
              return "result";
            }
          })()())({
            reflectSymbol: function() {
              return "initialValue";
            }
          })()())()))(gDecodeJsonCons(decodeFieldId(decodeRecord(gDecodeJsonCons(decodeFieldId(decodeJsonString))(gDecodeJsonCons(decodeFieldMaybe(decodeJsonEither(decodeJsonString)(decodeJsonString)))(gDecodeJsonCons(decodeFieldId(decodeJsonString))(gDecodeJsonNil)({
            reflectSymbol: function() {
              return "value";
            }
          })()())({
            reflectSymbol: function() {
              return "result";
            }
          })()())({
            reflectSymbol: function() {
              return "initialValue";
            }
          })()())()))(gDecodeJsonNil)({
            reflectSymbol: function() {
              return "state";
            }
          })()())({
            reflectSymbol: function() {
              return "nickname";
            }
          })()())({
            reflectSymbol: function() {
              return "name";
            }
          })()())({
            reflectSymbol: function() {
              return "city";
            }
          })()())()))(bindFlipped(bindEither)(parseJson)(note(new TypeMismatch2("No data"))(storedState)));
          if (v1 instanceof Left) {
            return log3(monadEffectHalogenM(monadEffectAff))(printJsonDecodeError(v1.value0));
          }
          ;
          if (v1 instanceof Right) {
            return bind(bindHalogenM)(gets(monadStateHalogenM)(function(v2) {
              return v2.formActions.setFields;
            }))(function(setFields) {
              return handleAction(setFields(v1.value0));
            });
          }
          ;
          throw new Error("Failed pattern match at Example.LocalStorage (line 59, column 7 - line 64, column 42): " + [v1.constructor.name]);
        });
      }
      ;
      if (v instanceof Receive7) {
        var fieldsJson = stringify(encodeJson(encodeRecord(gEncodeJsonCons(encodeRecord(gEncodeJsonCons(encodeJsonJString)(gEncodeJsonCons(encodeJsonMaybe(encodeJsonEither(encodeJsonJString)(encodeJsonJString)))(gEncodeJsonCons(encodeJsonJString)(gEncodeJsonNil)({
          reflectSymbol: function() {
            return "value";
          }
        })())({
          reflectSymbol: function() {
            return "result";
          }
        })())({
          reflectSymbol: function() {
            return "initialValue";
          }
        })())())(gEncodeJsonCons(encodeRecord(gEncodeJsonCons(encodeJsonJString)(gEncodeJsonCons(encodeJsonMaybe(encodeJsonEither(encodeJsonJString)(encodeJsonJString)))(gEncodeJsonCons(encodeJsonJString)(gEncodeJsonNil)({
          reflectSymbol: function() {
            return "value";
          }
        })())({
          reflectSymbol: function() {
            return "result";
          }
        })())({
          reflectSymbol: function() {
            return "initialValue";
          }
        })())())(gEncodeJsonCons(encodeRecord(gEncodeJsonCons(encodeJsonJString)(gEncodeJsonCons(encodeJsonMaybe(encodeJsonEither(encodeJsonJString)(encodeJsonJString)))(gEncodeJsonCons(encodeJsonJString)(gEncodeJsonNil)({
          reflectSymbol: function() {
            return "value";
          }
        })())({
          reflectSymbol: function() {
            return "result";
          }
        })())({
          reflectSymbol: function() {
            return "initialValue";
          }
        })())())(gEncodeJsonCons(encodeRecord(gEncodeJsonCons(encodeJsonJString)(gEncodeJsonCons(encodeJsonMaybe(encodeJsonEither(encodeJsonJString)(encodeJsonJString)))(gEncodeJsonCons(encodeJsonJString)(gEncodeJsonNil)({
          reflectSymbol: function() {
            return "value";
          }
        })())({
          reflectSymbol: function() {
            return "result";
          }
        })())({
          reflectSymbol: function() {
            return "initialValue";
          }
        })())())(gEncodeJsonNil)({
          reflectSymbol: function() {
            return "state";
          }
        })())({
          reflectSymbol: function() {
            return "nickname";
          }
        })())({
          reflectSymbol: function() {
            return "name";
          }
        })())({
          reflectSymbol: function() {
            return "city";
          }
        })())())(v.value0.fields));
        return discard(discardUnit)(bindHalogenM)(liftEffect(monadEffectHalogenM(monadEffectAff))(bindFlipped(bindEffect)(setItem("local-storage-form")(fieldsJson))(bindFlipped(bindEffect)(localStorage)(windowImpl))))(function() {
          return put(monadStateHalogenM)(v.value0);
        });
      }
      ;
      if (v instanceof Eval6) {
        return $$eval(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Example.LocalStorage (line 56, column 18 - line 72, column 20): " + [v.constructor.name]);
    };
    return formless(monadEffectAff)(mkFieldStates1(hmapRecord()(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "city";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "name";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "nickname";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "state";
      }
    })(constMapping(mappingMkFieldStateFieldS))(mapRecordWithIndexNil)()())()())()())()())))(mkFieldActions1(hmapWithIndexRecord()(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "city";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "city";
      }
    })(refl)())(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "name";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "name";
      }
    })(refl)())(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "nickname";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "nickname";
      }
    })(refl)())(mapRecordWithIndexCons({
      reflectSymbol: function() {
        return "state";
      }
    })(mappingWithIndexMkFieldAc({
      reflectSymbol: function() {
        return "state";
      }
    })(refl)())(mapRecordWithIndexNil)()())()())()())()())))(mkFieldResults1(hfoldlRecordWithIndex()(foldlRecordCons({
      reflectSymbol: function() {
        return "city";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "city";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "name";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "name";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "nickname";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "nickname";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "state";
      }
    })()(foldingWithIndexMkFieldRe({
      reflectSymbol: function() {
        return "state";
      }
    })(refl)()())(foldlRecordNil)))))))(mkFieldOutputs1(hfoldlRecordWithIndex()(foldlRecordCons({
      reflectSymbol: function() {
        return "city";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "city";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "name";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "name";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "nickname";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "nickname";
      }
    })(refl)()())(foldlRecordCons({
      reflectSymbol: function() {
        return "state";
      }
    })()(foldingWithIndexMkFieldOu({
      reflectSymbol: function() {
        return "state";
      }
    })(refl)()())(foldlRecordNil)))))))(mkConfig1(defaultsRecord()()))({
      liftAction: Eval6.create
    })(mempty(monoidRecord()(monoidRecordCons({
      reflectSymbol: function() {
        return "city";
      }
    })(monoidString)()(monoidRecordCons({
      reflectSymbol: function() {
        return "name";
      }
    })(monoidString)()(monoidRecordCons({
      reflectSymbol: function() {
        return "nickname";
      }
    })(monoidString)()(monoidRecordCons({
      reflectSymbol: function() {
        return "state";
      }
    })(monoidString)()(monoidRecordNil)))))))(mkComponent({
      initialState: identity(categoryFn),
      render: render3,
      "eval": mkEval({
        handleAction,
        handleQuery: handleQuery2,
        receive: function($15) {
          return Just.create(Receive7.create($15));
        },
        initialize: new Just(Initialize3.value),
        finalize: defaultEval.finalize
      })
    }));
  }();

  // output/Halogen.Aff.Util/index.js
  var selectElement = function(query2) {
    return bind(bindAff)(liftEffect(monadEffectAff)(bindFlipped(bindEffect)(composeKleisliFlipped(bindEffect)(function() {
      var $2 = querySelector(query2);
      return function($3) {
        return $2(toParentNode($3));
      };
    }())(document))(windowImpl)))(function(mel) {
      return pure(applicativeAff)(bindFlipped(bindMaybe)(fromElement)(mel));
    });
  };
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do2() {
      var rs = bindFlipped(bindEffect)(readyState)(bindFlipped(bindEffect)(document)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map(functorEffect)(toEventTarget)(windowImpl)();
        var listener = eventListener(function(v) {
          return callback(new Right(unit));
        })();
        addEventListener2(domcontentloaded)(listener)(false)(et)();
        return effectCanceler(removeEventListener2(domcontentloaded)(listener)(false)(et));
      }
      ;
      callback(new Right(unit))();
      return nonCanceler;
    };
  });

  // output/Halogen.Storybook.Proxy/index.js
  var _child = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var render = function(innerComponent) {
    return function(state3) {
      return slot_()({
        reflectSymbol: function() {
          return "child";
        }
      })(ordUnit)(_child)(unit)(innerComponent)(state3);
    };
  };
  var proxy = function(innerComponent) {
    return mkComponent({
      initialState: identity(categoryFn),
      render: render(innerComponent),
      "eval": mkEval(defaultEval)
    });
  };

  // output/Control.Monad.Fork.Class/index.js
  var monadForkAff = {
    suspend: suspendAff,
    fork: forkAff,
    join: joinFiber,
    Monad0: function() {
      return monadAff;
    },
    Functor1: function() {
      return functorFiber;
    }
  };
  var fork2 = function(dict) {
    return dict.fork;
  };

  // output/Halogen.Aff.Driver.State/index.js
  var unRenderStateX = unsafeCoerce2;
  var unDriverStateX = unsafeCoerce2;
  var renderStateX_ = function(dictApplicative) {
    return function(f) {
      return unDriverStateX(function(st) {
        return traverse_(dictApplicative)(foldableMaybe)(f)(st.rendering);
      });
    };
  };
  var mkRenderStateX = unsafeCoerce2;
  var renderStateX = function(dictFunctor) {
    return function(f) {
      return unDriverStateX(function(st) {
        return mkRenderStateX(f(st.rendering));
      });
    };
  };
  var mkDriverStateXRef = unsafeCoerce2;
  var mapDriverState = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var initDriverState = function(component) {
    return function(input3) {
      return function(handler3) {
        return function(lchs) {
          return function __do2() {
            var selfRef = $$new({})();
            var childrenIn = $$new(empty4)();
            var childrenOut = $$new(empty4)();
            var handlerRef = $$new(handler3)();
            var pendingQueries = $$new(new Just(Nil.value))();
            var pendingOuts = $$new(new Just(Nil.value))();
            var pendingHandlers = $$new(Nothing.value)();
            var fresh2 = $$new(1)();
            var subscriptions = $$new(new Just(empty3))();
            var forks = $$new(empty3)();
            var ds = {
              component,
              state: component.initialState(input3),
              refs: empty3,
              children: empty4,
              childrenIn,
              childrenOut,
              selfRef,
              handlerRef,
              pendingQueries,
              pendingOuts,
              pendingHandlers,
              rendering: Nothing.value,
              fresh: fresh2,
              subscriptions,
              forks,
              lifecycleHandlers: lchs
            };
            write(ds)(selfRef)();
            return mkDriverStateXRef(selfRef);
          };
        };
      };
    };
  };

  // output/Halogen.Aff.Driver.Eval/index.js
  var unsubscribe3 = function(sid) {
    return function(ref2) {
      return function __do2() {
        var v = read(ref2)();
        var subs = read(v.subscriptions)();
        return traverse_(applicativeEffect)(foldableMaybe)(unsubscribe)(bindFlipped(bindMaybe)(lookup3(ordSubscriptionId)(sid))(subs))();
      };
    };
  };
  var queueOrRun = function(ref2) {
    return function(au) {
      return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v) {
        if (v instanceof Nothing) {
          return au;
        }
        ;
        if (v instanceof Just) {
          return liftEffect(monadEffectAff)(write(new Just(new Cons(au, v.value0)))(ref2));
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
      });
    };
  };
  var handleLifecycle = function(lchs) {
    return function(f) {
      return discard(discardUnit)(bindAff)(liftEffect(monadEffectAff)(write({
        initializers: Nil.value,
        finalizers: Nil.value
      })(lchs)))(function() {
        return bind(bindAff)(liftEffect(monadEffectAff)(f))(function(result) {
          return bind(bindAff)(liftEffect(monadEffectAff)(read(lchs)))(function(v) {
            return discard(discardUnit)(bindAff)(traverse_(applicativeAff)(foldableList)(fork2(monadForkAff))(v.finalizers))(function() {
              return discard(discardUnit)(bindAff)(parSequence_(parallelAff)(foldableList)(v.initializers))(function() {
                return pure(applicativeAff)(result);
              });
            });
          });
        });
      });
    };
  };
  var handleAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit))));
  var fresh = function(f) {
    return function(ref2) {
      return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v) {
        return liftEffect(monadEffectAff)(modify$prime(function(i2) {
          return {
            state: i2 + 1 | 0,
            value: f(i2)
          };
        })(v.fresh));
      });
    };
  };
  var evalQ = function(render3) {
    return function(ref2) {
      return function(q2) {
        return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v) {
          return evalM(render3)(ref2)(v["component"]["eval"](new Query(map(functorCoyoneda)(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
        });
      };
    };
  };
  var evalM = function(render3) {
    return function(initRef) {
      return function(v) {
        var evalChildQuery = function(ref2) {
          return function(cqb) {
            return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v1) {
              return unChildQueryBox(function(v2) {
                var evalChild = function(v3) {
                  return parallel(parallelAff)(bind(bindAff)(liftEffect(monadEffectAff)(read(v3)))(function(dsx) {
                    return unDriverStateX(function(ds) {
                      return evalQ(render3)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };
                return map(functorAff)(v2.value2)(sequential(parallelAff)(v2.value0(applicativeParAff)(evalChild)(v1.children)));
              })(cqb);
            });
          };
        };
        var go2 = function(ref2) {
          return function(v1) {
            if (v1 instanceof State) {
              return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                var v3 = v1.value0(v2.state);
                if (unsafeRefEq(v2.state)(v3.value1)) {
                  return pure(applicativeAff)(v3.value0);
                }
                ;
                if (otherwise) {
                  return discard(discardUnit)(bindAff)(liftEffect(monadEffectAff)(write({
                    component: v2.component,
                    state: v3.value1,
                    refs: v2.refs,
                    children: v2.children,
                    childrenIn: v2.childrenIn,
                    childrenOut: v2.childrenOut,
                    selfRef: v2.selfRef,
                    handlerRef: v2.handlerRef,
                    pendingQueries: v2.pendingQueries,
                    pendingOuts: v2.pendingOuts,
                    pendingHandlers: v2.pendingHandlers,
                    rendering: v2.rendering,
                    fresh: v2.fresh,
                    subscriptions: v2.subscriptions,
                    forks: v2.forks,
                    lifecycleHandlers: v2.lifecycleHandlers
                  })(ref2)))(function() {
                    return discard(discardUnit)(bindAff)(handleLifecycle(v2.lifecycleHandlers)(render3(v2.lifecycleHandlers)(ref2)))(function() {
                      return pure(applicativeAff)(v3.value0);
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
              });
            }
            ;
            if (v1 instanceof Subscribe) {
              return bind(bindAff)(fresh(SubscriptionId)(ref2))(function(sid) {
                return bind(bindAff)(liftEffect(monadEffectAff)(subscribe(v1.value0(sid))(function(act) {
                  return handleAff(evalF(render3)(ref2)(new Action(act)));
                })))(function(finalize) {
                  return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                    return discard(discardUnit)(bindAff)(liftEffect(monadEffectAff)(modify_2(map(functorMaybe)(insert4(ordSubscriptionId)(sid)(finalize)))(v2.subscriptions)))(function() {
                      return pure(applicativeAff)(v1.value1(sid));
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Unsubscribe) {
              return discard(discardUnit)(bindAff)(liftEffect(monadEffectAff)(unsubscribe3(v1.value0)(ref2)))(function() {
                return pure(applicativeAff)(v1.value1);
              });
            }
            ;
            if (v1 instanceof Lift2) {
              return v1.value0;
            }
            ;
            if (v1 instanceof ChildQuery2) {
              return evalChildQuery(ref2)(v1.value0);
            }
            ;
            if (v1 instanceof Raise) {
              return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                return bind(bindAff)(liftEffect(monadEffectAff)(read(v2.handlerRef)))(function(handler3) {
                  return discard(discardUnit)(bindAff)(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                    return pure(applicativeAff)(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Par) {
              return sequential(parallelAff)(retractFreeAp(applicativeParAff)(hoistFreeAp(function() {
                var $83 = parallel(parallelAff);
                var $84 = evalM(render3)(ref2);
                return function($85) {
                  return $83($84($85));
                };
              }())(v1.value0)));
            }
            ;
            if (v1 instanceof Fork) {
              return bind(bindAff)(fresh(ForkId)(ref2))(function(fid) {
                return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                  return bind(bindAff)(liftEffect(monadEffectAff)($$new(false)))(function(doneRef) {
                    return bind(bindAff)(fork2(monadForkAff)($$finally(liftEffect(monadEffectAff)(function __do2() {
                      modify_2($$delete2(ordForkId)(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render3)(ref2)(v1.value0))))(function(fiber) {
                      return discard(discardUnit)(bindAff)(liftEffect(monadEffectAff)(unlessM(monadEffect)(read(doneRef))(modify_2(insert4(ordForkId)(fid)(fiber))(v2.forks))))(function() {
                        return pure(applicativeAff)(v1.value1(fid));
                      });
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Join) {
              return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                return bind(bindAff)(liftEffect(monadEffectAff)(read(v2.forks)))(function(forkMap) {
                  return discard(discardUnit)(bindAff)(traverse_(applicativeAff)(foldableMaybe)(joinFiber)(lookup3(ordForkId)(v1.value0)(forkMap)))(function() {
                    return pure(applicativeAff)(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Kill) {
              return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                return bind(bindAff)(liftEffect(monadEffectAff)(read(v2.forks)))(function(forkMap) {
                  return discard(discardUnit)(bindAff)(traverse_(applicativeAff)(foldableMaybe)(killFiber(error("Cancelled")))(lookup3(ordForkId)(v1.value0)(forkMap)))(function() {
                    return pure(applicativeAff)(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof GetRef) {
              return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v2) {
                return pure(applicativeAff)(v1.value1(lookup3(ordString)(v1.value0)(v2.refs)));
              });
            }
            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 139, column 33): " + [v1.constructor.name]);
          };
        };
        return foldFree(monadRecAff)(go2(initRef))(v);
      };
    };
  };
  var evalF = function(render3) {
    return function(ref2) {
      return function(v) {
        if (v instanceof RefUpdate) {
          return liftEffect(monadEffectAff)(flip(modify_2)(ref2)(mapDriverState(function(st) {
            return {
              component: st.component,
              state: st.state,
              refs: alter(ordString)($$const(v.value1))(v.value0)(st.refs),
              children: st.children,
              childrenIn: st.childrenIn,
              childrenOut: st.childrenOut,
              selfRef: st.selfRef,
              handlerRef: st.handlerRef,
              pendingQueries: st.pendingQueries,
              pendingOuts: st.pendingOuts,
              pendingHandlers: st.pendingHandlers,
              rendering: st.rendering,
              fresh: st.fresh,
              subscriptions: st.subscriptions,
              forks: st.forks,
              lifecycleHandlers: st.lifecycleHandlers
            };
          })));
        }
        ;
        if (v instanceof Action) {
          return bind(bindAff)(liftEffect(monadEffectAff)(read(ref2)))(function(v1) {
            return evalM(render3)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
          });
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
      };
    };
  };

  // output/Halogen.Aff.Driver/index.js
  var newLifecycleHandlers = /* @__PURE__ */ function() {
    return $$new({
      initializers: Nil.value,
      finalizers: Nil.value
    });
  }();
  var handlePending = function(ref2) {
    return function __do2() {
      var queue = read(ref2)();
      write(Nothing.value)(ref2)();
      return for_(applicativeEffect)(foldableMaybe)(queue)(function() {
        var $28 = traverse_(applicativeAff)(foldableList)(fork2(monadForkAff));
        return function($29) {
          return handleAff($28(reverse2($29)));
        };
      }())();
    };
  };
  var cleanupSubscriptionsAndForks = function(v) {
    return function __do2() {
      bindFlipped(bindEffect)(traverse_(applicativeEffect)(foldableMaybe)(traverse_(applicativeEffect)(foldableMap)(unsubscribe)))(read(v.subscriptions))();
      write(Nothing.value)(v.subscriptions)();
      bindFlipped(bindEffect)(traverse_(applicativeEffect)(foldableMap)(function() {
        var $30 = killFiber(error("finalized"));
        return function($31) {
          return handleAff($30($31));
        };
      }()))(read(v.forks))();
      return write(empty3)(v.forks)();
    };
  };
  var runUI = function(renderSpec2) {
    return function(component) {
      return function(i2) {
        var squashChildInitializers = function(lchs) {
          return function(preInits) {
            return unDriverStateX(function(st) {
              var parentInitializer = evalM(render3)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
              return modify_2(function(handlers) {
                return {
                  initializers: new Cons(discard(discardUnit)(bindAff)(parSequence_(parallelAff)(foldableList)(reverse2(handlers.initializers)))(function() {
                    return discard(discardUnit)(bindAff)(parentInitializer)(function() {
                      return liftEffect(monadEffectAff)(function __do2() {
                        handlePending(st.pendingQueries)();
                        return handlePending(st.pendingOuts)();
                      });
                    });
                  }), preInits),
                  finalizers: handlers.finalizers
                };
              })(lchs);
            });
          };
        };
        var runComponent = function(lchs) {
          return function(handler3) {
            return function(j) {
              return unComponent(function(c) {
                return function __do2() {
                  var lchs$prime = newLifecycleHandlers();
                  var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                  var pre2 = read(lchs)();
                  write({
                    initializers: Nil.value,
                    finalizers: pre2.finalizers
                  })(lchs)();
                  bindFlipped(bindEffect)(unDriverStateX(function() {
                    var $32 = render3(lchs);
                    return function($33) {
                      return $32(function(v) {
                        return v.selfRef;
                      }($33));
                    };
                  }()))(read($$var2))();
                  bindFlipped(bindEffect)(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
                  return $$var2;
                };
              });
            };
          };
        };
        var renderChild = function(lchs) {
          return function(handler3) {
            return function(childrenInRef) {
              return function(childrenOutRef) {
                return unComponentSlot(function(slot2) {
                  return function __do2() {
                    var childrenIn = map(functorEffect)(slot2.pop)(read(childrenInRef))();
                    var $$var2 = function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do3() {
                            flip(write)(st.handlerRef)(function() {
                              var $34 = maybe(pure(applicativeAff)(unit))(handler3);
                              return function($35) {
                                return $34(slot2.output($35));
                              };
                            }())();
                            return handleAff(evalM(render3)(st.selfRef)(st["component"]["eval"](new Receive(slot2.input, unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }
                      ;
                      if (childrenIn instanceof Nothing) {
                        return runComponent(lchs)(function() {
                          var $36 = maybe(pure(applicativeAff)(unit))(handler3);
                          return function($37) {
                            return $36(slot2.output($37));
                          };
                        }())(slot2.input)(slot2.component)();
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                    }();
                    var isDuplicate = map(functorEffect)(function($38) {
                      return isJust(slot2.get($38));
                    })(read(childrenOutRef))();
                    when(applicativeEffect)(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    modify_2(slot2.set($$var2))(childrenOutRef)();
                    return bind(bindEffect)(read($$var2))(renderStateX(functorEffect)(function(v) {
                      if (v instanceof Nothing) {
                        return $$throw("Halogen internal error: child was not initialized in renderChild");
                      }
                      ;
                      if (v instanceof Just) {
                        return pure(applicativeEffect)(renderSpec2.renderChild(v.value0));
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 227, column 37 - line 229, column 50): " + [v.constructor.name]);
                    }))();
                  };
                });
              };
            };
          };
        };
        var render3 = function(lchs) {
          return function($$var2) {
            return function __do2() {
              var v = read($$var2)();
              var shouldProcessHandlers = map(functorEffect)(isNothing)(read(v.pendingHandlers))();
              when(applicativeEffect)(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty4)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var selfRef = identity(categoryFn)(v.selfRef);
              var pendingQueries = identity(categoryFn)(v.pendingQueries);
              var pendingHandlers = identity(categoryFn)(v.pendingHandlers);
              var handler3 = function() {
                var $39 = queueOrRun(pendingHandlers);
                var $40 = $$void(functorAff);
                var $41 = evalF(render3)(selfRef);
                return function($42) {
                  return $39($40($41($42)));
                };
              }();
              var childHandler = function() {
                var $43 = queueOrRun(pendingQueries);
                return function($44) {
                  return $43(handler3(Action.create($44)));
                };
              }();
              var rendering = renderSpec2.render(function($45) {
                return handleAff(handler3($45));
              })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
              var children2 = read(v.childrenOut)();
              var childrenIn = read(v.childrenIn)();
              foreachSlot(applicativeEffect)(childrenIn)(function(v1) {
                return function __do3() {
                  var childDS = read(v1)();
                  renderStateX_(applicativeEffect)(renderSpec2.removeChild)(childDS)();
                  return finalize(lchs)(childDS)();
                };
              })();
              flip(modify_2)(v.selfRef)(mapDriverState(function(ds$prime) {
                return {
                  component: ds$prime.component,
                  state: ds$prime.state,
                  refs: ds$prime.refs,
                  children: children2,
                  childrenIn: ds$prime.childrenIn,
                  childrenOut: ds$prime.childrenOut,
                  selfRef: ds$prime.selfRef,
                  handlerRef: ds$prime.handlerRef,
                  pendingQueries: ds$prime.pendingQueries,
                  pendingOuts: ds$prime.pendingOuts,
                  pendingHandlers: ds$prime.pendingHandlers,
                  rendering: new Just(rendering),
                  fresh: ds$prime.fresh,
                  subscriptions: ds$prime.subscriptions,
                  forks: ds$prime.forks,
                  lifecycleHandlers: ds$prime.lifecycleHandlers
                };
              }))();
              return when(applicativeEffect)(shouldProcessHandlers)(flip(tailRecM(monadRecEffect))(unit)(function(v1) {
                return function __do3() {
                  var handlers = read(pendingHandlers)();
                  write(new Just(Nil.value))(pendingHandlers)();
                  traverse_(applicativeEffect)(foldableMaybe)(function() {
                    var $46 = traverse_(applicativeAff)(foldableList)(fork2(monadForkAff));
                    return function($47) {
                      return handleAff($46(reverse2($47)));
                    };
                  }())(handlers)();
                  var mmore = read(pendingHandlers)();
                  var $21 = maybe(false)($$null)(mmore);
                  if ($21) {
                    return voidLeft(functorEffect)(write(Nothing.value)(pendingHandlers))(new Done(unit))();
                  }
                  ;
                  return new Loop(unit);
                };
              }))();
            };
          };
        };
        var finalize = function(lchs) {
          return unDriverStateX(function(st) {
            return function __do2() {
              cleanupSubscriptionsAndForks(st)();
              var f = evalM(render3)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
              modify_2(function(handlers) {
                return {
                  initializers: handlers.initializers,
                  finalizers: new Cons(f, handlers.finalizers)
                };
              })(lchs)();
              return foreachSlot(applicativeEffect)(st.children)(function(v) {
                return function __do3() {
                  var dsx = read(v)();
                  return finalize(lchs)(dsx)();
                };
              })();
            };
          });
        };
        var evalDriver = function(disposed) {
          return function(ref2) {
            return function(q2) {
              return bind(bindAff)(liftEffect(monadEffectAff)(read(disposed)))(function(v) {
                if (v) {
                  return pure(applicativeAff)(Nothing.value);
                }
                ;
                return evalQ(render3)(ref2)(q2);
              });
            };
          };
        };
        var dispose = function(disposed) {
          return function(lchs) {
            return function(dsx) {
              return handleLifecycle(lchs)(function __do2() {
                var v = read(disposed)();
                if (v) {
                  return unit;
                }
                ;
                write(true)(disposed)();
                finalize(lchs)(dsx)();
                return unDriverStateX(function(v1) {
                  return function __do3() {
                    var v2 = liftEffect(monadEffectEffect)(read(v1.selfRef))();
                    return for_(applicativeEffect)(foldableMaybe)(v2.rendering)(renderSpec2.dispose)();
                  };
                })(dsx)();
              });
            };
          };
        };
        return bind(bindAff)(liftEffect(monadEffectAff)(newLifecycleHandlers))(function(lchs) {
          return bind(bindAff)(liftEffect(monadEffectAff)($$new(false)))(function(disposed) {
            return handleLifecycle(lchs)(function __do2() {
              var sio = create();
              var dsx = bindFlipped(bindEffect)(read)(runComponent(lchs)(function() {
                var $48 = liftEffect(monadEffectAff);
                var $49 = notify(sio.listener);
                return function($50) {
                  return $48($49($50));
                };
              }())(i2)(component))();
              return unDriverStateX(function(st) {
                return pure(applicativeEffect)({
                  query: evalDriver(disposed)(st.selfRef),
                  messages: sio.emitter,
                  dispose: dispose(disposed)(lchs)(dsx)
                });
              })(dsx)();
            });
          });
        });
      };
    };
  };

  // output/Web.DOM.Node/foreign.js
  var getEffProp2 = function(name17) {
    return function(node) {
      return function() {
        return node[name17];
      };
    };
  };
  var baseURI = getEffProp2("baseURI");
  var _ownerDocument = getEffProp2("ownerDocument");
  var _parentNode = getEffProp2("parentNode");
  var _parentElement = getEffProp2("parentElement");
  var childNodes = getEffProp2("childNodes");
  var _firstChild = getEffProp2("firstChild");
  var _lastChild = getEffProp2("lastChild");
  var _previousSibling = getEffProp2("previousSibling");
  var _nextSibling = getEffProp2("nextSibling");
  var _nodeValue = getEffProp2("nodeValue");
  var textContent = getEffProp2("textContent");
  function insertBefore(node1) {
    return function(node2) {
      return function(parent2) {
        return function() {
          parent2.insertBefore(node1, node2);
        };
      };
    };
  }
  function appendChild(node) {
    return function(parent2) {
      return function() {
        parent2.appendChild(node);
      };
    };
  }
  function removeChild2(node) {
    return function(parent2) {
      return function() {
        parent2.removeChild(node);
      };
    };
  }

  // output/Web.DOM.Node/index.js
  var parentNode2 = /* @__PURE__ */ function() {
    var $2 = map(functorEffect)(toMaybe);
    return function($3) {
      return $2(_parentNode($3));
    };
  }();
  var nextSibling = /* @__PURE__ */ function() {
    var $13 = map(functorEffect)(toMaybe);
    return function($14) {
      return $13(_nextSibling($14));
    };
  }();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy8 = function(name17, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name17 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var substInParent = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof Just && v2 instanceof Just) {
          return $$void(functorEffect)(insertBefore(v)(v1.value0)(v2.value0));
        }
        ;
        if (v1 instanceof Nothing && v2 instanceof Just) {
          return $$void(functorEffect)(appendChild(v)(v2.value0));
        }
        ;
        return pure(applicativeEffect)(unit);
      };
    };
  };
  var removeChild3 = function(v) {
    return function __do2() {
      var npn = parentNode2(v.node)();
      return traverse_(applicativeEffect)(foldableMaybe)(function(pn) {
        return removeChild2(v.node)(pn);
      })(npn)();
    };
  };
  var mkSpec = function(handler3) {
    return function(renderChildRef) {
      return function(document2) {
        var getNode = unRenderStateX(function(v) {
          return v.node;
        });
        var done = function(st) {
          if (st instanceof Just) {
            return halt(st.value0);
          }
          ;
          return unit;
        };
        var buildWidget2 = function(spec) {
          var buildThunk2 = buildThunk(unwrap())(spec);
          var $lazy_patch = $runtime_lazy8("patch", "Halogen.VDom.Driver", function() {
            return function(st, slot2) {
              if (st instanceof Just) {
                if (slot2 instanceof ComponentSlot) {
                  halt(st.value0);
                  return $lazy_renderComponentSlot(100)(slot2.value0);
                }
                ;
                if (slot2 instanceof ThunkSlot) {
                  var step$prime = step2(st.value0, slot2.value0);
                  return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                }
                ;
                throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot2.constructor.name]);
              }
              ;
              return $lazy_render(104)(slot2);
            };
          });
          var $lazy_render = $runtime_lazy8("render", "Halogen.VDom.Driver", function() {
            return function(slot2) {
              if (slot2 instanceof ComponentSlot) {
                return $lazy_renderComponentSlot(86)(slot2.value0);
              }
              ;
              if (slot2 instanceof ThunkSlot) {
                var step4 = buildThunk2(slot2.value0);
                return mkStep(new Step(extract2(step4), new Just(step4), $lazy_patch(89), done));
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot2.constructor.name]);
            };
          });
          var $lazy_renderComponentSlot = $runtime_lazy8("renderComponentSlot", "Halogen.VDom.Driver", function() {
            return function(cs) {
              var renderChild = read(renderChildRef)();
              var rsx = renderChild(cs)();
              var node = getNode(rsx);
              return mkStep(new Step(node, Nothing.value, $lazy_patch(117), done));
            };
          });
          var patch = $lazy_patch(91);
          var render3 = $lazy_render(82);
          var renderComponentSlot = $lazy_renderComponentSlot(109);
          return render3;
        };
        var buildAttributes = buildProp(handler3);
        return {
          buildWidget: buildWidget2,
          buildAttributes,
          document: document2
        };
      };
    };
  };
  var renderSpec = function(document2) {
    return function(container) {
      var render3 = function(handler3) {
        return function(child) {
          return function(v) {
            return function(v1) {
              if (v1 instanceof Nothing) {
                return function __do2() {
                  var renderChildRef = $$new(child)();
                  var spec = mkSpec(handler3)(renderChildRef)(document2);
                  var machine = buildVDom(spec)(v);
                  var node = extract2(machine);
                  $$void(functorEffect)(appendChild(node)(toNode2(container)))();
                  return {
                    machine,
                    node,
                    renderChildRef
                  };
                };
              }
              ;
              if (v1 instanceof Just) {
                return function __do2() {
                  write(child)(v1.value0.renderChildRef)();
                  var parent2 = parentNode2(v1.value0.node)();
                  var nextSib = nextSibling(v1.value0.node)();
                  var machine$prime = step2(v1.value0.machine, v);
                  var newNode = extract2(machine$prime);
                  when(applicativeEffect)(not(heytingAlgebraFunction(heytingAlgebraFunction(heytingAlgebraBoolean)))(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent2))();
                  return {
                    machine: machine$prime,
                    node: newNode,
                    renderChildRef: v1.value0.renderChildRef
                  };
                };
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 157, column 5 - line 173, column 80): " + [v1.constructor.name]);
            };
          };
        };
      };
      return {
        render: render3,
        renderChild: identity(categoryFn),
        removeChild: removeChild3,
        dispose: removeChild3
      };
    };
  };
  var runUI2 = function(component) {
    return function(i2) {
      return function(element3) {
        return bind(bindAff)(liftEffect(monadEffectAff)(map(functorEffect)(toDocument)(bindFlipped(bindEffect)(document)(windowImpl))))(function(document2) {
          return runUI(renderSpec(document2)(element3))(component)(i2);
        });
      };
    };
  };

  // output/JSURI/foreign.js
  function toRFC3896(input3) {
    return input3.replace(/[!'()*]/g, function(c) {
      return "%" + c.charCodeAt(0).toString(16);
    });
  }
  var _encodeURIComponent = function encode(fail2, succeed, input3) {
    try {
      return succeed(toRFC3896(encodeURIComponent(input3)));
    } catch (err) {
      return fail2(err);
    }
  };
  function _decodeURIComponent(fail2, succeed, input3) {
    try {
      return succeed(decodeURIComponent(input3));
    } catch (err) {
      return fail2(err);
    }
  }

  // output/JSURI/index.js
  var $$encodeURIComponent = /* @__PURE__ */ function() {
    return runFn3(_encodeURIComponent)($$const(Nothing.value))(Just.create);
  }();
  var $$decodeURIComponent = /* @__PURE__ */ function() {
    return runFn3(_decodeURIComponent)($$const(Nothing.value))(Just.create);
  }();

  // output/Web.HTML.Event.HashChangeEvent.EventTypes/index.js
  var hashchange = "hashchange";

  // output/Routing.Hash/index.js
  var getHash = /* @__PURE__ */ bind(bindEffect)(/* @__PURE__ */ bind(bindEffect)(windowImpl)(location))(/* @__PURE__ */ function() {
    var $2 = map(functorEffect)(function() {
      var $4 = fromMaybe("");
      var $5 = stripPrefix("#");
      return function($6) {
        return $4($5($6));
      };
    }());
    return function($3) {
      return $2(hash($3));
    };
  }());
  var foldHashes = function(cb) {
    return function(init3) {
      return function __do2() {
        var ref2 = bindFlipped(bindEffect)($$new)(bindFlipped(bindEffect)(init3)(getHash))();
        var win = map(functorEffect)(toEventTarget)(windowImpl)();
        var listener = eventListener(function(v) {
          return bindFlipped(bindEffect)(flip(write)(ref2))(join(bindEffect)(apply(applyEffect)(map(functorEffect)(cb)(read(ref2)))(getHash)));
        })();
        addEventListener2(hashchange)(listener)(false)(win)();
        return removeEventListener2(hashchange)(listener)(false)(win);
      };
    };
  };
  var matchesWith = function(dictFoldable) {
    return function(parser) {
      return function(cb) {
        var go2 = function(a2) {
          var $7 = maybe(pure(applicativeEffect)(a2))(function(b2) {
            return voidRight(functorEffect)(new Just(b2))(cb(a2)(b2));
          });
          var $8 = indexl(dictFoldable)(0);
          return function($9) {
            return $7($8(parser($9)));
          };
        };
        return foldHashes(go2)(go2(Nothing.value));
      };
    };
  };
  var hashes = /* @__PURE__ */ function() {
    return matchesWith(foldableMaybe)(Just.create);
  }();

  // output/Halogen.Storybook/index.js
  var RouteChange = /* @__PURE__ */ function() {
    function RouteChange2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RouteChange2.create = function(value0) {
      return function(value1) {
        return new RouteChange2(value0, value1);
      };
    };
    return RouteChange2;
  }();
  var handleQuery = function(v) {
    return discard(discardUnit)(bindHalogenM)($$void(functorHalogenM)(modify(monadStateHalogenM)(function(state3) {
      var $13 = {};
      for (var $14 in state3) {
        if ({}.hasOwnProperty.call(state3, $14)) {
          $13[$14] = state3[$14];
        }
        ;
      }
      ;
      $13.route = v.value0;
      return $13;
    })))(function() {
      return pure(applicativeHalogenM)(new Just(v.value1));
    });
  };
  var class_2 = function($40) {
    return class_(ClassName($40));
  };
  var renderStoryNames = function(v) {
    return function(items2) {
      var linkActiveClass = "Storybook-link is-active";
      return ul([class_2("Storybook-nav-list")])(mapFlipped(functorArray)(sortWith(ordString)(function(v1) {
        return v1.name;
      })(items2))(function(item2) {
        return li_([a([class_2(function() {
          var $20 = v.route === item2.path;
          if ($20) {
            return linkActiveClass;
          }
          ;
          return "Storybook-link";
        }()), href("#" + fromJust()($$encodeURIComponent(item2.path)))])([text(item2.name)])]);
      }));
    };
  };
  var renderSidebar = function(stories2) {
    return function(state3) {
      var nameObj = foldMapDefaultL(foldableArray)(monoidObject(semigroupArray))(function(v) {
        if (v === "") {
          return empty2;
        }
        ;
        var v1 = indexOf2("|")(v);
        if (v1 instanceof Nothing) {
          return singleton3("")([{
            name: v,
            path: v
          }]);
        }
        ;
        if (v1 instanceof Just) {
          var v2 = splitAt2(v1.value0)(v);
          return singleton3(v2.before)([{
            name: drop4(1)(v2.after),
            path: v
          }]);
        }
        ;
        throw new Error("Failed pattern match at Halogen.Storybook (line 104, column 7 - line 110, column 50): " + [v1.constructor.name]);
      })(keys(stories2));
      var sorted = sortWith(ordString)(fst)(toUnfoldable2(unfoldableArray)(nameObj));
      return div2([class_2("Storybook-nav")])(mapFlipped(functorArray)(sorted)(function(v) {
        if (v.value0 === "") {
          return div2([class_2("Storybook-nav-section")])([renderStoryNames(state3)(v.value1)]);
        }
        ;
        return div2([class_2("Storybook-nav-section")])([div2([class_2("Storybook-nav-section-title")])([text(v.value0)]), renderStoryNames(state3)(v.value1)]);
      }));
    };
  };
  var _child2 = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var renderMain = function(stories2) {
    return function(state3) {
      var v = lookup2(state3.route)(stories2);
      if (v instanceof Just) {
        return slot()({
          reflectSymbol: function() {
            return "child";
          }
        })(ordString)(_child2)(state3.route)(v.value0)(unit)(absurd);
      }
      ;
      return div_([h2_([text("Hello world")]), p_([text("This site is powered by "), a([href("https://github.com/rnons/purescript-halogen-storybook")])([text("halogen-storybook")]), text(".")])]);
    };
  };
  var render2 = function(v) {
    return function(state3) {
      return div2([class_2("Storybook")])([a([class_2("Storybook-logo"), href("#")])([function() {
        if (v.logo instanceof Nothing) {
          return text("Halogen Storybook");
        }
        ;
        if (v.logo instanceof Just) {
          return fromPlainHTML(v.logo.value0);
        }
        ;
        throw new Error("Failed pattern match at Halogen.Storybook (line 136, column 7 - line 138, column 45): " + [v.logo.constructor.name]);
      }()]), renderSidebar(v.stories)(state3), div2([class_2("Storybook-main")])([renderMain(v.stories)(state3)])]);
    };
  };
  var app = function(config) {
    var initialState = {
      route: ""
    };
    return mkComponent({
      initialState: $$const(initialState),
      render: render2(config),
      "eval": mkEval({
        handleAction: defaultEval.handleAction,
        handleQuery,
        receive: defaultEval.receive,
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize
      })
    });
  };
  var runStorybook = function(config) {
    return function(body2) {
      return bind(bindAff)(runUI2(app(config))(unit)(body2))(function(app$prime) {
        return $$void(functorAff)(liftEffect(monadEffectAff)(hashes(function(v) {
          return function(next) {
            return for_(applicativeEffect)(foldableMaybe)($$decodeURIComponent(next))(function(next$prime) {
              return launchAff(app$prime.query(mkTell(RouteChange.create(next$prime))));
            });
          };
        })));
      });
    };
  };

  // output/Example.Main/index.js
  var mkExample = function(dictShow) {
    return function(title4) {
      return function(description) {
        return function(formComponent) {
          var render3 = function(state3) {
            return article_([h1_([text(title4)]), p_([text(description)]), slot()({
              reflectSymbol: function() {
                return "inner";
              }
            })(ordUnit)($$Proxy.value)(unit)(formComponent)(unit)(identity(categoryFn)), function() {
              if (state3.result instanceof Nothing) {
                return text("");
              }
              ;
              if (state3.result instanceof Just) {
                return code_([text(show(dictShow)(state3.result.value0))]);
              }
              ;
              throw new Error("Failed pattern match at Example.Main (line 111, column 9 - line 113, column 60): " + [state3.result.constructor.name]);
            }()]);
          };
          var handleAction = function(result) {
            return modify_(monadStateHalogenM)(function(v) {
              var $7 = {};
              for (var $8 in v) {
                if ({}.hasOwnProperty.call(v, $8)) {
                  $7[$8] = v[$8];
                }
                ;
              }
              ;
              $7.result = new Just(result);
              return $7;
            });
          };
          return mkComponent({
            initialState: function(v) {
              return {
                result: Nothing.value
              };
            },
            render: render3,
            "eval": mkEval({
              handleAction,
              handleQuery: defaultEval.handleQuery,
              receive: defaultEval.receive,
              initialize: defaultEval.initialize,
              finalize: defaultEval.finalize
            })
          });
        };
      };
    };
  };
  var stories = /* @__PURE__ */ function() {
    var localStorage2 = function() {
      var component = mkExample(showRecord()()(showRecordFieldsCons({
        reflectSymbol: function() {
          return "city";
        }
      })(showRecordFieldsCons({
        reflectSymbol: function() {
          return "name";
        }
      })(showRecordFieldsCons({
        reflectSymbol: function() {
          return "nickname";
        }
      })(showRecordFieldsCons({
        reflectSymbol: function() {
          return "state";
        }
      })(showRecordFieldsNil)(showString))(showString))(showString))(showString)))("Local Storage")("A form that persists its state to local storage. Click the submit button and then refresh the page! Useful to see how to imperatively set the form state.")(form6);
      return new Tuple("5. Local Storage", proxy(component));
    }();
    var home = function() {
      var title4 = h1_([text("Formless")]);
      var description = p_([text("A simple library for writing forms in Halogen")]);
      var render3 = article_([title4, description]);
      var component = mkComponent({
        initialState: identity(categoryFn),
        render: function(v) {
          return render3;
        },
        "eval": mkEval(defaultEval)
      });
      return new Tuple("", proxy(component));
    }();
    var fileUpload2 = function() {
      var component = mkExample(showRecord()()(showRecordFieldsCons({
        reflectSymbol: function() {
          return "name";
        }
      })(showRecordFieldsCons({
        reflectSymbol: function() {
          return "photo";
        }
      })(showRecordFieldsNil)(showRecord()()(showRecordFieldsCons({
        reflectSymbol: function() {
          return "name";
        }
      })(showRecordFieldsCons({
        reflectSymbol: function() {
          return "size";
        }
      })(showRecordFieldsNil)(showNumber))(showString))))(showString)))("File Upload")("A form with a file upload button and several validation functions. Useful to see how to implement more complex form fields and validation.")(form5);
      return new Tuple("3. File Upload", proxy(component));
    }();
    var dependentFields = function() {
      var component = mkExample(showRecord()()(showRecordFieldsCons({
        reflectSymbol: function() {
          return "email";
        }
      })(showRecordFieldsCons({
        reflectSymbol: function() {
          return "password";
        }
      })(showRecordFieldsCons({
        reflectSymbol: function() {
          return "username";
        }
      })(showRecordFieldsNil)(showUsername))(showString))(showEmail)))("Dependent Fields")("A form with fields that can set other field values and fields that mutually depend on each other for validation. Useful to see how to implement validation that accesses the form state and performs effects, as well as how to imperatively modify field states.")(form4);
      return new Tuple("4. Dependent Fields", proxy(component));
    }();
    var checkboxRadio = function() {
      var component = mkExample(showRecord()()(showRecordFieldsCons({
        reflectSymbol: function() {
          return "name";
        }
      })(showRecordFieldsCons({
        reflectSymbol: function() {
          return "picked";
        }
      })(showRecordFieldsCons({
        reflectSymbol: function() {
          return "subscribe";
        }
      })(showRecordFieldsNil)(showBoolean))(showPicked))(showString)))("Checkbox & Radio")("A form demonstrating how to use common form controls like checkboxes and radio groups. Useful to see how to implement your own form field helpers for your application.")(form3);
      return new Tuple("2. Checkbox & Radio", proxy(component));
    }();
    var basic = function() {
      var component = mkExample(showRecord()()(showRecordFieldsCons({
        reflectSymbol: function() {
          return "message";
        }
      })(showRecordFieldsCons({
        reflectSymbol: function() {
          return "name";
        }
      })(showRecordFieldsNil)(showString))(showString)))("Basic")("A simple form that implements all fields manually, without app-specific helpers. Useful to see exactly how Formless should be used when implementing your own helper functions for your application.")(form2);
      return new Tuple("1. Basic", proxy(component));
    }();
    return fromFoldable2(foldableArray)([home, basic, checkboxRadio, fileUpload2, dependentFields, localStorage2]);
  }();
  var main2 = /* @__PURE__ */ launchAff_(/* @__PURE__ */ bind(bindAff)(awaitLoad)(function() {
    return bind(bindAff)(selectElement(".app"))(function(mbApp) {
      return for_(applicativeAff)(foldableMaybe)(mbApp)(function(app2) {
        return runStorybook({
          stories,
          logo: new Just(text("Formless"))
        })(app2);
      });
    });
  }));

  // <stdin>
  main2();
})();
