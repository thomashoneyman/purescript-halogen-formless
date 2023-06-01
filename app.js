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
    var compose12 = compose(dictSemigroupoid);
    return function(f) {
      return function(g) {
        return compose12(g)(f);
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
    var map111 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map111(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map111 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map111($$const(x))(f);
      };
    };
  };
  var voidRight = function(dictFunctor) {
    var map111 = map(dictFunctor);
    return function(x) {
      return map111($$const(x));
    };
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map29 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map29($$const(identity2))(a2))(b2);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure16(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure16(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply4 = apply(dictApplicative.Apply0());
    var pure16 = pure(dictApplicative);
    return function(f) {
      return function(a2) {
        return apply4(pure16(f))(a2);
      };
    };
  };

  // output/Control.Bind/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
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
    var bindFlipped14 = bindFlipped(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bindFlipped14(f)(g(a2));
        };
      };
    };
  };
  var composeKleisli = function(dictBind) {
    var bind17 = bind(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bind17(f(a2))(g);
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
    var bind17 = bind(dictBind);
    return function(m) {
      return bind17(m)(identity3);
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
    return function(eq3) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq3 : gt;
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
    return function(value14) {
      return function(rec) {
        var copy2 = {};
        for (var key in rec) {
          if ({}.hasOwnProperty.call(rec, key)) {
            copy2[key] = rec[key];
          }
        }
        copy2[label5] = value14;
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
  var eq2 = /* @__PURE__ */ eq(eqBoolean);
  var notEq = function(dictEq) {
    var eq3 = eq(dictEq);
    return function(x) {
      return function(y) {
        return eq2(eq3(x)(y))(false);
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
    var compare3 = compare(dictOrd);
    return function(f) {
      return function(x) {
        return function(y) {
          return compare3(f(x))(f(y));
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
        var showRecordFields1 = showRecordFields(dictShowRecordFields);
        return {
          show: function(record) {
            var v = showRecordFields1($$Proxy.value)(record);
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
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShowRecordFields) {
      var showRecordFields1 = showRecordFields(dictShowRecordFields);
      return function(dictShow) {
        var show12 = show(dictShow);
        return {
          showRecordFields: function(v) {
            return function(record) {
              var tail2 = showRecordFields1($$Proxy.value)(record);
              var key = reflectSymbol2($$Proxy.value);
              var focus3 = unsafeGet(key)(record);
              return cons(intercalate(": ")([key, show12(focus3)]))(tail2);
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
    var ff1 = ff(dictHeytingAlgebra);
    var tt1 = tt(dictHeytingAlgebra);
    var implies1 = implies(dictHeytingAlgebra);
    var conj1 = conj(dictHeytingAlgebra);
    var disj1 = disj(dictHeytingAlgebra);
    var not1 = not(dictHeytingAlgebra);
    return {
      ff: function(v) {
        return ff1;
      },
      tt: function(v) {
        return tt1;
      },
      implies: function(f) {
        return function(g) {
          return function(a2) {
            return implies1(f(a2))(g(a2));
          };
        };
      },
      conj: function(f) {
        return function(g) {
          return function(a2) {
            return conj1(f(a2))(g(a2));
          };
        };
      },
      disj: function(f) {
        return function(g) {
          return function(a2) {
            return disj1(f(a2))(g(a2));
          };
        };
      },
      not: function(f) {
        return function(a2) {
          return not1(f(a2));
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
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function(dictSemigroupRecord) {
        var appendRecord1 = appendRecord(dictSemigroupRecord);
        return function(dictSemigroup) {
          var append12 = append(dictSemigroup);
          return {
            appendRecord: function(v) {
              return function(ra) {
                return function(rb) {
                  var tail2 = appendRecord1($$Proxy.value)(ra)(rb);
                  var key = reflectSymbol2($$Proxy.value);
                  var insert10 = unsafeSet(key);
                  var get4 = unsafeGet(key);
                  return insert10(append12(get4(ra))(get4(rb)))(tail2);
                };
              };
            }
          };
        };
      };
    };
  };

  // output/Data.Monoid/index.js
  var semigroupRecord2 = /* @__PURE__ */ semigroupRecord();
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
      var semigroupRecord1 = semigroupRecord2(dictMonoidRecord.SemigroupRecord0());
      return {
        mempty: memptyRecord(dictMonoidRecord)($$Proxy.value),
        Semigroup0: function() {
          return semigroupRecord1;
        }
      };
    };
  };
  var mempty = function(dict) {
    return dict.mempty;
  };
  var monoidRecordCons = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    var semigroupRecordCons2 = semigroupRecordCons(dictIsSymbol)();
    return function(dictMonoid) {
      var mempty1 = mempty(dictMonoid);
      var Semigroup0 = dictMonoid.Semigroup0();
      return function() {
        return function(dictMonoidRecord) {
          var memptyRecord1 = memptyRecord(dictMonoidRecord);
          var semigroupRecordCons1 = semigroupRecordCons2(dictMonoidRecord.SemigroupRecord0())(Semigroup0);
          return {
            memptyRecord: function(v) {
              var tail2 = memptyRecord1($$Proxy.value);
              var key = reflectSymbol2($$Proxy.value);
              var insert10 = unsafeSet(key);
              return insert10(mempty1)(tail2);
            },
            SemigroupRecord0: function() {
              return semigroupRecordCons1;
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
    var eq3 = eq(dictEq);
    return function(dictEq1) {
      var eq12 = eq(dictEq1);
      return {
        eq: function(x) {
          return function(y) {
            return eq3(x.value0)(y.value0) && eq12(x.value1)(y.value1);
          };
        }
      };
    };
  };
  var ordTuple = function(dictOrd) {
    var compare2 = compare(dictOrd);
    var eqTuple1 = eqTuple(dictOrd.Eq0());
    return function(dictOrd1) {
      var compare12 = compare(dictOrd1);
      var eqTuple2 = eqTuple1(dictOrd1.Eq0());
      return {
        compare: function(x) {
          return function(y) {
            var v = compare2(x.value0)(y.value0);
            if (v instanceof LT) {
              return LT.value;
            }
            ;
            if (v instanceof GT) {
              return GT.value;
            }
            ;
            return compare12(x.value1)(y.value1);
          };
        },
        Eq0: function() {
          return eqTuple2;
        }
      };
    };
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var put = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(s) {
      return state1(function(v) {
        return new Tuple(unit, s);
      });
    };
  };
  var modify_ = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(unit, f(s));
      });
    };
  };
  var modify = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        var s$prime = f(s);
        return new Tuple(s$prime, s$prime);
      });
    };
  };
  var gets = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
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
  var identity4 = /* @__PURE__ */ identity(categoryFn);
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
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a2) {
    return maybe(a2)(identity4);
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
          return map2(v.value0)(v1);
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
  var map3 = /* @__PURE__ */ map(functorEither);
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
          return map3(v.value0)(v1);
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
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var bimap = function(dict) {
    return dict.bimap;
  };
  var lmap = function(dictBifunctor) {
    var bimap1 = bimap(dictBifunctor);
    return function(f) {
      return bimap1(f)(identity5);
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
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure16 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond2(f($454));
        })(pure16(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_14 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_14(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var indexl = function(dictFoldable) {
    var foldl2 = foldl(dictFoldable);
    return function(idx) {
      var go2 = function(cursor) {
        return function(a2) {
          if (cursor.elem instanceof Just) {
            return cursor;
          }
          ;
          var $296 = cursor.pos === idx;
          if ($296) {
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
      var $455 = foldl2(go2)({
        elem: Nothing.value,
        pos: 0
      });
      return function($456) {
        return function(v) {
          return v.elem;
        }($455($456));
      };
    };
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v2.value0)(v1);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v1)(v2.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty2;
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
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append6 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append6(f(x))(acc);
          };
        })(mempty2);
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
    var foldl2 = foldl(dictFoldable);
    return function(dictMonoid) {
      var append6 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl2(function(acc) {
          return function(x) {
            return append6(acc)(f(x));
          };
        })(mempty2);
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
                  step4 = sequential3(util, supervisor, step4._1);
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
      function onComplete(join5) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join5.rethrow;
            join5.handler(step4)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join5;
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
      function join4(cb) {
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
        join: join4,
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
      function join4(result, head4, tail2) {
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
                        join4(fail2, null, null);
                      } else {
                        join4(fail2, tail2._1, tail2._2);
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
                        join4(step4, null, null);
                      } else {
                        join4(step4, tail2._1, tail2._2);
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
            join4(result, fiber._2._1, fiber._2._2);
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
    function sequential3(util, supervisor, par) {
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
        return Aff.Bind(aff, function(value14) {
          return Aff.Pure(f(value14));
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
    var bind17 = bind(dictMonad.Bind1());
    var unless2 = unless(dictMonad.Applicative0());
    return function(mb) {
      return function(m) {
        return bind17(mb)(function(b2) {
          return unless2(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    var bind17 = bind(dictMonad.Bind1());
    var pure16 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a2) {
        return bind17(f)(function(f$prime) {
          return bind17(a2)(function(a$prime) {
            return pure16(f$prime(a$prime));
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
  var $$throw = function($4) {
    return throwException(error($4));
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map29 = map(Monad0.Bind1().Apply0().Functor0());
    var pure16 = pure(Monad0.Applicative0());
    return function(a2) {
      return catchError1(map29(Right.create)(a2))(function($52) {
        return pure16(Left.create($52));
      });
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
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
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
      return $$void2(modify2(f)(s));
    };
  };

  // output/Control.Monad.Rec.Class/index.js
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
  var map4 = /* @__PURE__ */ map(functorEffect);
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
          var r = bindFlipped2($$new)(f(a2))();
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
          return map4(fromDone)(read(r))();
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
  var map5 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x) {
    return x;
  };
  var runExceptT = function(v) {
    return v;
  };
  var monadTransExceptT = {
    lift: function(dictMonad) {
      var bind17 = bind(dictMonad.Bind1());
      var pure16 = pure(dictMonad.Applicative0());
      return function(m) {
        return bind17(m)(function(a2) {
          return pure16(new Right(a2));
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
    var map111 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map111(map5(f)));
      }
    };
  };
  var except = function(dictApplicative) {
    var $185 = pure(dictApplicative);
    return function($186) {
      return ExceptT($185($186));
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
    var bind17 = bind(dictMonad.Bind1());
    var pure16 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind17(v)(either(function($187) {
            return pure16(Left.create($187));
          })(function(a2) {
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
    var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: function() {
        var $188 = pure(dictMonad.Applicative0());
        return function($189) {
          return ExceptT($188(Right.create($189)));
        };
      }(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: function() {
        var $198 = pure(dictMonad.Applicative0());
        return function($199) {
          return ExceptT($198(Left.create($199)));
        };
      }(),
      Monad0: function() {
        return monadExceptT1;
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
    return function(apply4) {
      return function(map29) {
        return function(pure16) {
          return function(f) {
            return function(array) {
              function go2(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure16([]);
                  case 1:
                    return map29(array1)(f(array[bot]));
                  case 2:
                    return apply4(map29(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply4(apply4(map29(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply4(map29(concat2)(go2(bot, pivot)))(go2(pivot, top3));
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
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var parTraverse_ = function(dictParallel) {
    var sequential3 = sequential(dictParallel);
    var traverse_7 = traverse_(dictParallel.Applicative1());
    var parallel3 = parallel(dictParallel);
    return function(dictFoldable) {
      var traverse_14 = traverse_7(dictFoldable);
      return function(f) {
        var $48 = traverse_14(function($50) {
          return parallel3(f($50));
        });
        return function($49) {
          return sequential3($48($49));
        };
      };
    };
  };
  var parSequence_ = function(dictParallel) {
    var parTraverse_1 = parTraverse_(dictParallel);
    return function(dictFoldable) {
      return parTraverse_1(dictFoldable)(identity6);
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
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
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
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void3 = /* @__PURE__ */ $$void(functorEffect);
  var map6 = /* @__PURE__ */ map(functorEffect);
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
  var map1 = /* @__PURE__ */ map(functorAff);
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
  var launchAff_ = function($73) {
    return $$void3(launchAff($73));
  };
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
  var pure22 = /* @__PURE__ */ pure(applicativeAff);
  var bind1 = /* @__PURE__ */ bind(bindAff);
  var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindAff);
  var $$finally = function(fin) {
    return function(a2) {
      return bracket(pure22(unit))($$const(fin))($$const(a2));
    };
  };
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var effectCanceler = function($74) {
    return Canceler($$const(liftEffect2($74)));
  };
  var joinFiber = function(v) {
    return makeAff(function(k) {
      return map6(effectCanceler)(v.join(k));
    });
  };
  var functorFiber = {
    map: function(f) {
      return function(t) {
        return unsafePerformEffect(makeFiber(map1(f)(joinFiber(t))));
      };
    }
  };
  var killFiber = function(e) {
    return function(v) {
      return bind1(liftEffect2(v.isSuspended))(function(suspended) {
        if (suspended) {
          return liftEffect2($$void3(v.kill(e, $$const(pure2(unit)))));
        }
        ;
        return makeAff(function(k) {
          return map6(effectCanceler)(v.kill(e, k));
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
  var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
  var runAff = function(k) {
    return function(aff) {
      return launchAff(bindFlipped3(function($77) {
        return liftEffect2(k($77));
      })($$try2(aff)));
    };
  };
  var runAff_ = function(k) {
    return function(aff) {
      return $$void3(runAff(k)(aff));
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
        var $79 = parallel(parallelAff);
        return function($80) {
          return $79(pure22($80));
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
        return bind1(k(a2))(function(res) {
          if (res instanceof Done) {
            return pure22(res.value0);
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
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure22(unit));

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
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function() {
        return function(l) {
          return function(a2) {
            return function(r) {
              return unsafeSet(reflectSymbol2(l))(a2)(r);
            };
          };
        };
      };
    };
  };
  var get2 = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function(l) {
        return function(r) {
          return unsafeGet(reflectSymbol2(l))(r);
        };
      };
    };
  };
  var $$delete = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function() {
        return function(l) {
          return function(r) {
            return unsafeDelete(reflectSymbol2(l))(r);
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
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        return function(l) {
          return function(f) {
            return function(r1) {
              return unsafeModify(reflectSymbol2(l))(f)(r1);
            };
          };
        };
      };
    };
  };
  var insert2 = function() {
    return function() {
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        return function(l) {
          return function(a2) {
            return function(r1) {
              return unsafeInsert(reflectSymbol2(l))(a2)(r1);
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
  var merge2 = /* @__PURE__ */ merge()();
  var defaultsRecord = function() {
    return function() {
      return {
        defaults: flip(merge2)
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
    return function(fromJust6) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value14 = b2;
              while (true) {
                var maybe2 = f(value14);
                if (isNothing2(maybe2))
                  return result;
                var tuple = fromJust6(maybe2);
                result.push(fst2(tuple));
                value14 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust6) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value14 = b2;
              while (true) {
                var tuple = f(value14);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2))
                  return result;
                value14 = fromJust6(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };
  var none = function(dictUnfoldable) {
    return unfoldr(dictUnfoldable)($$const(Nothing.value))(unit);
  };

  // output/Data.Enum/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var toEnumWithDefaults = function(dictBoundedEnum) {
    var toEnum1 = toEnum(dictBoundedEnum);
    var fromEnum1 = fromEnum(dictBoundedEnum);
    var bottom1 = bottom(dictBoundedEnum.Bounded0());
    return function(low2) {
      return function(high2) {
        return function(x) {
          var v = toEnum1(x);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $140 = x < fromEnum1(bottom1);
            if ($140) {
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
    if (v >= bottom2 && v <= top2) {
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
    var empty7 = empty(dictPlus);
    return function(a2) {
      return new NonEmpty(a2, empty7);
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
    var chunkedRevMap = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
            $tco_var_v = new Cons(v1, v);
            $copy_v1 = v1.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v2) {
            if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
              return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
            }
            ;
            if (v2 instanceof Cons && v2.value1 instanceof Nil) {
              return new Cons(f(v2.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v2) {
            return function($copy_v3) {
              var $tco_var_v2 = $copy_v2;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v2, v3) {
                if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v2 = v2.value1;
                  $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                  return;
                }
                ;
                $tco_done1 = true;
                return v3;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(v)(unrolledMap(v1));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
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
          var go2 = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = new Cons(v1.value0, v);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $284 = foldl(foldableList)(flip(f))(b2);
        return function($285) {
          return $284(rev3($285));
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
      var append22 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append22(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty2);
      };
    }
  };
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr2(Cons.create)(ys)(xs);
      };
    }
  };
  var append1 = /* @__PURE__ */ append(semigroupList);
  var altList = {
    alt: append1,
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
            var mapFlipped4 = mapFlipped(dictFunctor);
            return function(r) {
              return function(k) {
                return function(v) {
                  if (unsafeHas(v.type)(r)) {
                    return mapFlipped4(unsafeGet(v.type)(r)(v.value))(function(value14) {
                      return {
                        type: v.type,
                        value: value14
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
  var traverseSome1 = /* @__PURE__ */ traverseSome()()()();
  var traverse2 = function() {
    return function() {
      return function() {
        return function() {
          return function(dictApplicative) {
            var traverseSome2 = traverseSome1(dictApplicative.Apply0().Functor0());
            var pure16 = pure(dictApplicative);
            return function(r) {
              return traverseSome2(r)(function($173) {
                return pure16($173);
              });
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
  var overSome1 = /* @__PURE__ */ overSome()()()();
  var over2 = function() {
    return function() {
      return function() {
        return function() {
          return function(r) {
            return overSome1(r)(unsafeCoerce2);
          };
        };
      };
    };
  };
  var inj = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(p2) {
        return function(value14) {
          return {
            type: reflectSymbol2(p2),
            value: value14
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
  function _foldM(bind17) {
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
              acc = bind17(acc)(g(k));
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
    return function(value14) {
      if (count < 1) {
        return [];
      }
      var result = new Array(count);
      return result.fill(value14);
    };
  };
  var replicatePolyfill = function(count) {
    return function(value14) {
      var result = [];
      var n = 0;
      for (var i2 = 0; i2 < count; i2++) {
        result[n++] = value14;
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
    return function(foldr4) {
      return function(xs) {
        return listToArray(foldr4(curryCons)(emptyList)(xs));
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
  var fromJust4 = /* @__PURE__ */ fromJust();
  var unsafeIndex = function() {
    return unsafeIndexImpl;
  };
  var unsafeIndex1 = /* @__PURE__ */ unsafeIndex();
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
    var unfoldr3 = unfoldr(dictUnfoldable);
    return function(xs) {
      var len = length(xs);
      var f = function(i2) {
        if (i2 < len) {
          return new Just(new Tuple(unsafeIndex1(xs)(i2), i2 + 1 | 0));
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Array (line 156, column 3 - line 158, column 26): " + [i2.constructor.name]);
      };
      return unfoldr3(f)(0);
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
    var comparing2 = comparing(dictOrd);
    return function(f) {
      return sortBy(comparing2(f));
    };
  };
  var fromFoldable = function(dictFoldable) {
    return fromFoldableImpl(foldr(dictFoldable));
  };
  var findIndex = /* @__PURE__ */ function() {
    return findIndexImpl(Just.create)(Nothing.value);
  }();
  var elemIndex = function(dictEq) {
    var eq22 = eq(dictEq);
    return function(x) {
      return findIndex(function(v) {
        return eq22(v)(x);
      });
    };
  };
  var elem2 = function(dictEq) {
    var elemIndex1 = elemIndex(dictEq);
    return function(a2) {
      return function(arr) {
        return isJust(elemIndex1(a2)(arr));
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
          return fromJust4(deleteAt(i2)(v2));
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
  var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindST);
  var $$void4 = /* @__PURE__ */ $$void(functorST);
  var toUnfoldable2 = function(dictUnfoldable) {
    var $86 = toUnfoldable(dictUnfoldable);
    var $87 = toArrayWithKey(Tuple.create);
    return function($88) {
      return $86($87($88));
    };
  };
  var thawST = _copyST;
  var singleton3 = function(k) {
    return function(v) {
      return runST(bindFlipped4(poke2(k)(v))(newImpl));
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
    var fromFoldable1 = fromFoldable(dictFoldable);
    return function(l) {
      return runST(function __do2() {
        var s = newImpl();
        foreach(fromFoldable1(l))(function(v) {
          return $$void4(poke2(v.value0)(v.value1)(s));
        })();
        return s;
      });
    };
  };
  var foldM = function(dictMonad) {
    var bind17 = bind(dictMonad.Bind1());
    var pure16 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(z) {
        return _foldM(bind17)(f)(pure16(z));
      };
    };
  };
  var foldM1 = /* @__PURE__ */ foldM(monadST);
  var unionWith = function(f) {
    return function(m1) {
      return function(m2) {
        return mutate(function(s1) {
          return foldM1(function(s2) {
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
    var semigroupObject1 = semigroupObject(dictSemigroup);
    return {
      mempty: empty2,
      Semigroup0: function() {
        return semigroupObject1;
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
          return coyoneda(function($180) {
            return f(v1.value0($180));
          })(v1.value1);
        })(v);
      };
    }
  };
  var liftCoyoneda = /* @__PURE__ */ coyoneda(/* @__PURE__ */ identity(categoryFn));

  // output/Data.List/index.js
  var reverse2 = /* @__PURE__ */ function() {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return v;
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_v = new Cons(v1.value0, v);
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
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
    var compare2 = compare(dictOrd);
    return function(k) {
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
            var v2 = compare2(k)(v.value1);
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
            var v3 = compare2(k)(v.value1);
            if (v3 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            var v4 = compare2(k)(v.value4);
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
      return function($copy_v1) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictOrd, v, v1) {
          if (v instanceof Nil) {
            $tco_done = true;
            return v1;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Two(v1, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }
            ;
            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Two(v.value0.value0, v.value0.value1, v.value0.value2, v1);
              return;
            }
            ;
            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v1, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v1, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, v1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
  };
  var insert4 = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare2 = compare(dictOrd);
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
                  return fromZipper1(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }
                ;
                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
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
        var down = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v2 instanceof Leaf) {
                $tco_done1 = true;
                return up(v1)(new KickUp(Leaf.value, k, v, Leaf.value));
              }
              ;
              if (v2 instanceof Two) {
                var v3 = compare2(k)(v2.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Two(v2.value0, k, v, v2.value3));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_v1 = new Cons(new TwoLeft(v2.value1, v2.value2, v2.value3), v1);
                  $copy_v2 = v2.value0;
                  return;
                }
                ;
                $tco_var_v1 = new Cons(new TwoRight(v2.value0, v2.value1, v2.value2), v1);
                $copy_v2 = v2.value3;
                return;
              }
              ;
              if (v2 instanceof Three) {
                var v3 = compare2(k)(v2.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Three(v2.value0, k, v, v2.value3, v2.value4, v2.value5, v2.value6));
                }
                ;
                var v4 = compare2(k)(v2.value4);
                if (v4 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, k, v, v2.value6));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_v1 = new Cons(new ThreeLeft(v2.value1, v2.value2, v2.value3, v2.value4, v2.value5, v2.value6), v1);
                  $copy_v2 = v2.value0;
                  return;
                }
                ;
                if (v3 instanceof GT && v4 instanceof LT) {
                  $tco_var_v1 = new Cons(new ThreeMiddle(v2.value0, v2.value1, v2.value2, v2.value4, v2.value5, v2.value6), v1);
                  $copy_v2 = v2.value3;
                  return;
                }
                ;
                $tco_var_v1 = new Cons(new ThreeRight(v2.value0, v2.value1, v2.value2, v2.value3, v2.value4, v2.value5), v1);
                $copy_v2 = v2.value6;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
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
    var fromZipper1 = fromZipper(dictOrd);
    var compare2 = compare(dictOrd);
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
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
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
                return fromZipper1(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
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
              var v = compare2(k)(m.value1);
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
              var v = compare2(k)(m.value4);
              var v3 = compare2(k)(m.value1);
              if (leaves && v3 instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, fromZipper1(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
              }
              ;
              if (leaves && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, fromZipper1(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
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
      var mempty2 = mempty(dictMonoid);
      var append22 = append(dictMonoid.Semigroup0());
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty2;
          }
          ;
          if (m instanceof Two) {
            return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(foldMap(foldableMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(append22(foldMap(foldableMap)(dictMonoid)(f)(m.value3))(append22(f(m.value5))(foldMap(foldableMap)(dictMonoid)(f)(m.value6)))));
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
    var pop12 = pop(dictOrd);
    return function(k) {
      return function(m) {
        return maybe(m)(snd)(pop12(k)(m));
      };
    };
  };
  var alter = function(dictOrd) {
    var lookup13 = lookup3(dictOrd);
    var delete1 = $$delete2(dictOrd);
    var insert13 = insert4(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = f(lookup13(k)(m));
          if (v instanceof Nothing) {
            return delete1(k)(m);
          }
          ;
          if (v instanceof Just) {
            return insert13(k)(v.value0)(m);
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
  var ordTuple2 = /* @__PURE__ */ ordTuple(ordString)(ordOrdBox);
  var pop1 = /* @__PURE__ */ pop(ordTuple2);
  var lookup1 = /* @__PURE__ */ lookup3(ordTuple2);
  var insert1 = /* @__PURE__ */ insert4(ordTuple2);
  var pop2 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key) {
            return function(v) {
              return pop1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(v);
            };
          };
        };
      };
    };
  };
  var lookup4 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key) {
            return function(v) {
              return lookup1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(v);
            };
          };
        };
      };
    };
  };
  var insert5 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key) {
            return function(val) {
              return function(v) {
                return insert1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(val)(v);
              };
            };
          };
        };
      };
    };
  };
  var foreachSlot = function(dictApplicative) {
    var traverse_7 = traverse_(dictApplicative)(foldableMap);
    return function(v) {
      return function(k) {
        return traverse_7(function($54) {
          return k($54);
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
  var map7 = /* @__PURE__ */ map(functorArray);
  var map12 = /* @__PURE__ */ map(functorTuple);
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
    return function($61) {
      return f($61);
    };
  };
  var graft = unsafeCoerce2;
  var bifunctorGraft = {
    bimap: function(f) {
      return function(g) {
        return unGraft(function(v) {
          return graft(new Graft(function($63) {
            return f(v.value0($63));
          }, function($64) {
            return g(v.value1($64));
          }, v.value2));
        });
      };
    }
  };
  var bimap2 = /* @__PURE__ */ bimap(bifunctorGraft);
  var runGraft = /* @__PURE__ */ unGraft(function(v) {
    var go2 = function(v2) {
      if (v2 instanceof Text) {
        return new Text(v2.value0);
      }
      ;
      if (v2 instanceof Elem) {
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map7(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map7(map12(go2))(v2.value3));
      }
      ;
      if (v2 instanceof Widget) {
        return new Widget(v.value1(v2.value0));
      }
      ;
      if (v2 instanceof Grafted) {
        return new Grafted(bimap2(v.value0)(v.value1)(v2.value0));
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
  var map8 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map8(toMaybe);
    var $3 = _querySelector(qs);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/Web.Internal.FFI/foreign.js
  function _unsafeReadProtoTagged(nothing, just, name17, value14) {
    if (typeof window !== "undefined") {
      var ty = window[name17];
      if (ty != null && value14 instanceof ty) {
        return just(value14);
      }
    }
    var obj = value14;
    while (obj != null) {
      var proto = Object.getPrototypeOf(obj);
      var constructorName = proto.constructor.name;
      if (constructorName === name17) {
        return just(value14);
      } else if (constructorName === "Object") {
        return nothing;
      }
      obj = proto;
    }
    return nothing;
  }

  // output/Web.Internal.FFI/index.js
  var unsafeReadProtoTagged = function(name17) {
    return function(value14) {
      return _unsafeReadProtoTagged(Nothing.value, Just.create, name17, value14);
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
    var $63 = v === v1;
    if ($63) {
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
  function typeOf(value14) {
    return typeof value14;
  }
  function tagOf(value14) {
    return Object.prototype.toString.call(value14).slice(8, -1);
  }
  var isArray = Array.isArray || function(value14) {
    return Object.prototype.toString.call(value14) === "[object Array]";
  };

  // output/Data.List.NonEmpty/index.js
  var singleton5 = /* @__PURE__ */ function() {
    var $200 = singleton2(plusList);
    return function($201) {
      return NonEmptyList($200($201));
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
      var $20 = v1.before === v;
      if ($20) {
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
    var $23 = indexOf(pat);
    return function($24) {
      return isJust($23($24));
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
    var $153 = throwError(monadThrowExceptT(dictMonad));
    return function($154) {
      return $153(singleton5($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure16 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail(dictMonad);
    return function(tag) {
      return function(value14) {
        if (tagOf(value14) === tag) {
          return pure16(unsafeFromForeign(value14));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag, tagOf(value14)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value14.constructor.name]);
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
            var $66 = v11.value2 === v2.value2;
            if ($66) {
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
              var $75 = refEq2(elVal, v2.value1);
              if ($75) {
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
  var widget = function($28) {
    return HTML(Widget.create($28));
  };
  var toPropValue = function(dict) {
    return dict.toPropValue;
  };
  var text = function($29) {
    return HTML(Text.create($29));
  };
  var prop = function(dictIsProp) {
    var toPropValue1 = toPropValue(dictIsProp);
    return function(v) {
      var $31 = Property.create(v);
      return function($32) {
        return $31(toPropValue1($32));
      };
    };
  };
  var isPropString = {
    toPropValue: propFromString
  };
  var isPropInputType = {
    toPropValue: function($45) {
      return propFromString(renderInputType($45));
    }
  };
  var isPropButtonType = {
    toPropValue: function($50) {
      return propFromString(renderButtonType($50));
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
  var identity7 = /* @__PURE__ */ identity(categoryFn);
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
  var goLeft = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(fStack) {
      return function(valStack) {
        return function(nat) {
          return function(func) {
            return function(count) {
              if (func instanceof Pure) {
                return new Tuple(new Cons({
                  func: pure16(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Lift) {
                return new Tuple(new Cons({
                  func: nat(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Ap) {
                return goLeft(dictApplicative)(fStack)(cons3(func.value1)(valStack))(nat)(func.value0)(count + 1 | 0);
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
            };
          };
        };
      };
    };
  };
  var goApply = function(dictApplicative) {
    var apply4 = apply(dictApplicative.Apply0());
    return function(fStack) {
      return function(vals) {
        return function(gVal) {
          if (fStack instanceof Nil) {
            return new Left(gVal);
          }
          ;
          if (fStack instanceof Cons) {
            var gRes = apply4(fStack.value0.func)(gVal);
            var $31 = fStack.value0.count === 1;
            if ($31) {
              if (fStack.value1 instanceof Nil) {
                return new Left(gRes);
              }
              ;
              return goApply(dictApplicative)(fStack.value1)(vals)(gRes);
            }
            ;
            if (vals instanceof Nil) {
              return new Left(gRes);
            }
            ;
            if (vals instanceof Cons) {
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
    var goApply1 = goApply(dictApplicative);
    var pure16 = pure(dictApplicative);
    var goLeft1 = goLeft(dictApplicative);
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply1(v.value0)(v.value1.value1)(pure16(v.value1.value0.value0));
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
              var v1 = goApply1(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));
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
              $copy_v = goLeft1(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
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
    return foldFreeAp(dictApplicative)(identity7);
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
  var foldFreeAp1 = /* @__PURE__ */ foldFreeAp(applicativeFreeAp);
  var hoistFreeAp = function(f) {
    return foldFreeAp1(function($54) {
      return liftFreeAp(f($54));
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
  var foldr3 = function(k) {
    return function(b2) {
      return function(q2) {
        var foldl2 = function($copy_v) {
          return function($copy_v1) {
            return function($copy_v2) {
              var $tco_var_v = $copy_v;
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1, v2) {
                if (v2 instanceof Nil) {
                  $tco_done = true;
                  return v1;
                }
                ;
                if (v2 instanceof Cons) {
                  $tco_var_v = v;
                  $tco_var_v1 = v(v1)(v2.value0);
                  $copy_v2 = v2.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
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
        var $66 = $$null2(v.value1);
        if ($66) {
          return CatNil.value;
        }
        ;
        return foldr3(link)(CatNil.value)(v.value1);
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
  var append3 = /* @__PURE__ */ append(semigroupCatList);
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
          return new Free(v22.value0, append3(v22.value1)(r));
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
          var $189 = pure(freeApplicative);
          return function($190) {
            return $189(k($190));
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
    pure: function($191) {
      return fromView(Return.create($191));
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
  var pure3 = /* @__PURE__ */ pure(freeApplicative);
  var liftF = function(f) {
    return fromView(new Bind(f, function($192) {
      return pure3($192);
    }));
  };
  var foldFree = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map111 = map(Monad0.Bind1().Apply0().Functor0());
    var pure16 = pure(Monad0.Applicative0());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map111(Done.create)(pure16(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map111(function($199) {
            return Loop.create(v.value1($199));
          })(k(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
      };
      return tailRecM4(go2);
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
  var $$void5 = /* @__PURE__ */ $$void(functorEffect);
  var bind2 = /* @__PURE__ */ bind(bindEffect);
  var append4 = /* @__PURE__ */ append(semigroupArray);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_1 = /* @__PURE__ */ traverse_2(foldableArray);
  var unsubscribe = function(v) {
    return v;
  };
  var subscribe = function(v) {
    return function(k) {
      return v(function($76) {
        return $$void5(k($76));
      });
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
            return append4(v)([k]);
          })(subscribers)();
          return modify_2(deleteBy(unsafeRefEq)(k))(subscribers);
        };
      },
      listener: function(a2) {
        return bind2(read(subscribers))(traverse_1(function(k) {
          return k(a2);
        }));
      }
    };
  };

  // output/Halogen.Query.HalogenM/index.js
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var lookup5 = /* @__PURE__ */ lookup4();
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
      var lookup13 = lookup5(dictIsSymbol);
      return function(dictOrd) {
        var lookup23 = lookup13(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(q2) {
              return liftF(new ChildQuery2(mkChildQueryBox(new ChildQuery(function(dictApplicative) {
                var pure16 = pure(dictApplicative);
                return function(k) {
                  var $177 = maybe(pure16(Nothing.value))(k);
                  var $178 = lookup23(label5)(p2);
                  return function($179) {
                    return $177($178($179));
                  };
                };
              }, q2, identity8))));
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
    state: function($181) {
      return HalogenM(liftF(State.create($181)));
    },
    Monad0: function() {
      return monadHalogenM;
    }
  };
  var monadEffectHalogenM = function(dictMonadEffect) {
    return {
      liftEffect: function() {
        var $186 = liftEffect(dictMonadEffect);
        return function($187) {
          return HalogenM(liftF(Lift2.create($186($187))));
        };
      }(),
      Monad0: function() {
        return monadHalogenM;
      }
    };
  };
  var monadAffHalogenM = function(dictMonadAff) {
    var monadEffectHalogenM1 = monadEffectHalogenM(dictMonadAff.MonadEffect0());
    return {
      liftAff: function() {
        var $188 = liftAff(dictMonadAff);
        return function($189) {
          return HalogenM(liftF(Lift2.create($188($189))));
        };
      }(),
      MonadEffect0: function() {
        return monadEffectHalogenM1;
      }
    };
  };
  var functorHalogenM = freeFunctor;
  var fork = function(hmu) {
    return liftF(new Fork(hmu, identity8));
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
        var $48 = unsafeEqThunk(state3.thunk, t2);
        if ($48) {
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
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorHalogenM);
  var traverse_3 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
  var map9 = /* @__PURE__ */ map(functorHalogenM);
  var pure4 = /* @__PURE__ */ pure(applicativeHalogenM);
  var lookup6 = /* @__PURE__ */ lookup4();
  var pop3 = /* @__PURE__ */ pop2();
  var insert6 = /* @__PURE__ */ insert5();
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
        return voidLeft2(traverse_3(args.handleAction)(args.initialize))(v.value0);
      }
      ;
      if (v instanceof Finalize) {
        return voidLeft2(traverse_3(args.handleAction)(args.finalize))(v.value0);
      }
      ;
      if (v instanceof Receive) {
        return voidLeft2(traverse_3(args.handleAction)(args.receive(v.value0)))(v.value1);
      }
      ;
      if (v instanceof Action2) {
        return voidLeft2(args.handleAction(v.value0))(v.value1);
      }
      ;
      if (v instanceof Query) {
        return unCoyoneda(function(g) {
          var $45 = map9(maybe(v.value1(unit))(g));
          return function($46) {
            return $45(args.handleQuery($46));
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
      handleAction: $$const(pure4(unit)),
      handleQuery: $$const(pure4(Nothing.value)),
      receive: $$const(Nothing.value),
      initialize: Nothing.value,
      finalize: Nothing.value
    };
  }();
  var componentSlot = function() {
    return function(dictIsSymbol) {
      var lookup13 = lookup6(dictIsSymbol);
      var pop12 = pop3(dictIsSymbol);
      var insert13 = insert6(dictIsSymbol);
      return function(dictOrd) {
        var lookup23 = lookup13(dictOrd);
        var pop22 = pop12(dictOrd);
        var insert22 = insert13(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(comp) {
              return function(input3) {
                return function(output2) {
                  return mkComponentSlot({
                    get: lookup23(label5)(p2),
                    pop: pop22(label5)(p2),
                    set: insert22(label5)(p2),
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
  var unwrap2 = /* @__PURE__ */ unwrap();
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var prop1 = /* @__PURE__ */ prop2(isPropBoolean);
  var prop22 = /* @__PURE__ */ prop2(isPropString);
  var type_ = function(dictIsProp) {
    return prop2(dictIsProp)("type");
  };
  var value = function(dictIsProp) {
    return prop2(dictIsProp)("value");
  };
  var placeholder = /* @__PURE__ */ prop22("placeholder");
  var name2 = /* @__PURE__ */ prop22("name");
  var href = /* @__PURE__ */ prop22("href");
  var $$for = /* @__PURE__ */ prop22("htmlFor");
  var class_ = /* @__PURE__ */ function() {
    var $36 = prop22("className");
    return function($37) {
      return $36(unwrap2($37));
    };
  }();
  var checked = /* @__PURE__ */ prop1("checked");
  var attr2 = /* @__PURE__ */ function() {
    return attr(Nothing.value);
  }();

  // output/Halogen.HTML/index.js
  var componentSlot2 = /* @__PURE__ */ componentSlot();
  var slot_ = function() {
    return function(dictIsSymbol) {
      var componentSlot1 = componentSlot2(dictIsSymbol);
      return function(dictOrd) {
        var componentSlot22 = componentSlot1(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(component) {
              return function(input3) {
                return widget(new ComponentSlot(componentSlot22(label5)(p2)(component)(input3)($$const(Nothing.value))));
              };
            };
          };
        };
      };
    };
  };
  var slot = function() {
    return function(dictIsSymbol) {
      var componentSlot1 = componentSlot2(dictIsSymbol);
      return function(dictOrd) {
        var componentSlot22 = componentSlot1(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(component) {
              return function(input3) {
                return function(outputQuery) {
                  return widget(new ComponentSlot(componentSlot22(label5)(p2)(component)(input3)(function($11) {
                    return Just.create(outputQuery($11));
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
  function _read(nothing, just, value14) {
    var tag = Object.prototype.toString.call(value14);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value14);
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
  var $$void6 = /* @__PURE__ */ $$void(functorHalogenM);
  var query2 = /* @__PURE__ */ query();
  var identity9 = /* @__PURE__ */ identity(categoryFn);
  var tell2 = function() {
    return function(dictIsSymbol) {
      var query1 = query2(dictIsSymbol);
      return function(dictOrd) {
        var query22 = query1(dictOrd);
        return function(slot5) {
          return function(label5) {
            return function(req) {
              return $$void6(query22(slot5)(label5)(req(unit)));
            };
          };
        };
      };
    };
  };
  var request = function() {
    return function(dictIsSymbol) {
      var query1 = query2(dictIsSymbol);
      return function(dictOrd) {
        var query22 = query1(dictOrd);
        return function(slot5) {
          return function(label5) {
            return function(req) {
              return query22(slot5)(label5)(req(identity9));
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
      var foldlRecordRowList1 = foldlRecordRowList(dictFoldlRecord);
      return {
        hfoldlWithIndex: function(f) {
          return function(x) {
            return foldlRecordRowList1(f)(x)($$Proxy.value);
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
    var get4 = get2(dictIsSymbol)();
    return function() {
      return function(dictFoldingWithIndex) {
        var foldingWithIndex1 = foldingWithIndex(dictFoldingWithIndex);
        return function(dictFoldlRecord) {
          var foldlRecordRowList1 = foldlRecordRowList(dictFoldlRecord);
          return {
            foldlRecordRowList: function(f) {
              return function(x) {
                return function(v) {
                  return function(r) {
                    return foldlRecordRowList1(f)(foldingWithIndex1(f)($$Proxy.value)(x)(get4($$Proxy.value)(r)))($$Proxy.value)(r);
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
  var identity10 = /* @__PURE__ */ identity(categoryBuilder);
  var compose1 = /* @__PURE__ */ compose(semigroupoidBuilder);
  var modify6 = /* @__PURE__ */ modify3()();
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
        return identity10;
      };
    }
  };
  var mapRecordWithIndexBuilder = function(dict) {
    return dict.mapRecordWithIndexBuilder;
  };
  var mapRecordWithIndexCons = function(dictIsSymbol) {
    var modify1 = modify6(dictIsSymbol);
    return function(dictMappingWithIndex) {
      var mappingWithIndex1 = mappingWithIndex(dictMappingWithIndex);
      return function(dictMapRecordWithIndex) {
        var mapRecordWithIndexBuilder1 = mapRecordWithIndexBuilder(dictMapRecordWithIndex);
        return function() {
          return function() {
            return {
              mapRecordWithIndexBuilder: function(v) {
                return function(f) {
                  return compose1(modify1($$Proxy.value)(mappingWithIndex1(f)($$Proxy.value)))(mapRecordWithIndexBuilder1($$Proxy.value)(f));
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
          var $102 = mapRecordWithIndexBuilder(dictMapRecordWithIndex)($$Proxy.value);
          return function($103) {
            return build($102($103));
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
          var $108 = mapRecordWithIndexBuilder(dictMapRecordWithIndex)($$Proxy.value);
          return function($109) {
            return build($108(ConstMapping($109)));
          };
        }()
      };
    };
  };
  var hmap = function(dict) {
    return dict.hmap;
  };
  var constMapping = function(dictMapping) {
    var mapping1 = mapping(dictMapping);
    return {
      mappingWithIndex: function(v) {
        return function(v1) {
          return mapping1(v);
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
  var target = function($3) {
    return toMaybe(_target($3));
  };
  var currentTarget = function($5) {
    return toMaybe(_currentTarget($5));
  };

  // output/Formless/index.js
  var map11 = /* @__PURE__ */ map(functorMaybe);
  var pure5 = /* @__PURE__ */ pure(applicativeMaybe);
  var identity11 = /* @__PURE__ */ identity(categoryBuilder);
  var inj3 = /* @__PURE__ */ inj();
  var apply2 = /* @__PURE__ */ apply(applyMaybe);
  var composeFlipped2 = /* @__PURE__ */ composeFlipped(semigroupoidBuilder);
  var insert7 = /* @__PURE__ */ insert2()();
  var traverse3 = /* @__PURE__ */ traverse2()()()();
  var over3 = /* @__PURE__ */ over2()()()();
  var discard2 = /* @__PURE__ */ discard(discardUnit)(bindHalogenM);
  var pure1 = /* @__PURE__ */ pure(applicativeHalogenM);
  var bind3 = /* @__PURE__ */ bind(bindHalogenM);
  var $$delete3 = /* @__PURE__ */ $$delete({
    reflectSymbol: function() {
      return "liftAction";
    }
  })()();
  var coerce3 = /* @__PURE__ */ coerce();
  var bindFlipped5 = /* @__PURE__ */ bindFlipped(bindHalogenM);
  var innerIsSymbol = {
    reflectSymbol: function() {
      return "inner";
    }
  };
  var request2 = /* @__PURE__ */ request()(innerIsSymbol)(ordUnit);
  var gets2 = /* @__PURE__ */ gets(monadStateHalogenM);
  var for_2 = /* @__PURE__ */ for_(applicativeHalogenM);
  var for_1 = /* @__PURE__ */ for_2(foldableMaybe);
  var modify_3 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var get3 = /* @__PURE__ */ get(monadStateHalogenM);
  var for_22 = /* @__PURE__ */ for_2(foldableArray);
  var tell3 = /* @__PURE__ */ tell2()(innerIsSymbol)(ordUnit);
  var map13 = /* @__PURE__ */ map(functorObject);
  var slot2 = /* @__PURE__ */ slot()(innerIsSymbol)(ordUnit);
  var query3 = /* @__PURE__ */ query()(innerIsSymbol)(ordUnit);
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
        var $267 = map11(flip(build)({}));
        var $268 = hfoldlWithIndex(dictHFoldlWithIndex)(MkFieldResult.value)(pure5(identity11));
        return function($269) {
          return $267($268($269));
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
        var $270 = map11(flip(build)({}));
        var $271 = hfoldlWithIndex(dictHFoldlWithIndex)(MkFieldOutput.value)(pure5(identity11));
        return function($272) {
          return $270($271($272));
        };
      }(),
      HFoldlWithIndex0: function() {
        return dictHFoldlWithIndex;
      }
    };
  };
  var mkFieldActions1 = function(dictHMapWithIndex) {
    var hmapWithIndex2 = hmapWithIndex(dictHMapWithIndex);
    return {
      mkFieldActions: function(lift4) {
        return hmapWithIndex2(lift4);
      },
      HMapWithIndex0: function() {
        return dictHMapWithIndex;
      }
    };
  };
  var mappingWithIndexMkFieldAc = function(dictIsSymbol) {
    var inj1 = inj3(dictIsSymbol);
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictTypeEquals) {
      return function() {
        return {
          mappingWithIndex: function(v) {
            return function(sym) {
              return function(v1) {
                var fieldVariant = inj1(sym)(unit);
                return {
                  key: reflectSymbol2(sym),
                  modify: function() {
                    var $273 = ModifyField.create(fieldVariant);
                    return function($274) {
                      return v($273($274));
                    };
                  }(),
                  reset: v(new ResetField(fieldVariant)),
                  validate: v(new ValidateField(fieldVariant)),
                  handleChange: function() {
                    var $275 = ChangeField.create(fieldVariant);
                    return function($276) {
                      return v($275($276));
                    };
                  }(),
                  handleBlur: function() {
                    var $277 = BlurField.create(fieldVariant);
                    return function($278) {
                      return v($277($278));
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
    var insert13 = insert7(dictIsSymbol);
    return function(dictTypeEquals) {
      var from2 = from(dictTypeEquals);
      return function() {
        return function() {
          return {
            foldingWithIndex: function(v) {
              return function(prop3) {
                return function(rin) {
                  return function(field) {
                    var v1 = from2(field);
                    return apply2(map11(composeFlipped2)(rin))(map11(insert13(prop3))(v1.result));
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
    var insert13 = insert7(dictIsSymbol);
    return function(dictTypeEquals) {
      var from2 = from(dictTypeEquals);
      return function() {
        return function() {
          return {
            foldingWithIndex: function(v) {
              return function(prop3) {
                return function(rin) {
                  return function(field) {
                    var result = from2(field);
                    return apply2(map11(composeFlipped2)(rin))(map11(insert13(prop3))(hush(result)));
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
    var pure23 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1(unit);
        }
        ;
        return pure23(unit);
      };
    };
  };
  var when$prime1 = /* @__PURE__ */ when$prime(applicativeHalogenM);
  var validateM = function() {
    return function() {
      return function() {
        return function() {
          return function(dictApplicative) {
            return flip(traverse3(dictApplicative));
          };
        };
      };
    };
  };
  var validate = function() {
    return function() {
      return function() {
        return function() {
          return flip(over3);
        };
      };
    };
  };
  var raise2 = function($279) {
    return raise(Raise2.create($279));
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
            return discard2(onSubmit2(v.value0))(function() {
              return pure1(new Just(v.value1));
            });
          }
          ;
          if (v instanceof Validate) {
            return bind3(validateM$prime(v.value0)(validators))(function(validated) {
              return pure1(new Just(v.value1(validated)));
            });
          }
          ;
          return pure1(Nothing.value);
        };
      };
    };
  };
  var handleSubmitValidate = function(onSubmit2) {
    return function(validate$prime) {
      return function(validators) {
        return function(v) {
          if (v instanceof Submit) {
            return discard2(onSubmit2(v.value0))(function() {
              return pure1(new Just(v.value1));
            });
          }
          ;
          if (v instanceof Validate) {
            return pure1(new Just(v.value1(validate$prime(v.value0)(validators))));
          }
          ;
          return pure1(Nothing.value);
        };
      };
    };
  };
  var formless = function(dictMonadEffect) {
    var liftEffect9 = liftEffect(monadEffectHalogenM(dictMonadEffect));
    return function(dictMkFieldStates) {
      var mkFieldStates2 = mkFieldStates(dictMkFieldStates);
      return function(dictMkFieldActions) {
        var mkFieldActions2 = mkFieldActions(dictMkFieldActions);
        return function(dictMkFieldResults) {
          var mkFieldResults2 = mkFieldResults(dictMkFieldResults);
          return function(dictMkFieldOutputs) {
            var mkFieldOutputs2 = mkFieldOutputs(dictMkFieldOutputs);
            return function(dictMkConfig) {
              var mkConfig2 = mkConfig(dictMkConfig);
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
                      var initialFullConfig = mkConfig2(providedConfig);
                      var initialFormState = {
                        submitCount: 0,
                        errorCount: 0,
                        allTouched: false
                      };
                      var initialFormActions = {
                        setFields: function($280) {
                          return initialFullConfig.liftAction(SetForm.create($280));
                        },
                        reset: initialFullConfig.liftAction(ResetForm.value),
                        setConfig: function($281) {
                          return initialFullConfig.liftAction(SetFormConfig.create($281));
                        },
                        submit: initialFullConfig.liftAction(new SubmitForm(Nothing.value)),
                        handleSubmit: function($282) {
                          return initialFullConfig.liftAction(SubmitForm.create(Just.create($282)));
                        }
                      };
                      var initialFieldStates = mkFieldStates2(initialForm);
                      var initialFieldActions = mkFieldActions2(initialFullConfig.liftAction)(initialFieldStates);
                      var initialConfig = $$delete3($$Proxy.value)(initialFullConfig);
                      return {
                        input: input3,
                        fieldObject: initialFieldStates,
                        fieldActions: initialFieldActions,
                        formState: initialFormState,
                        formActions: initialFormActions,
                        formConfig: initialConfig
                      };
                    };
                    var getKeys = function($283) {
                      return coerce3(keys($283));
                    };
                    var getField = function(v) {
                      return function(object2) {
                        var field = unsafeIndex2(object2)(v);
                        return {
                          type: v,
                          value: field.value
                        };
                      };
                    };
                    var fieldsKey = function($284) {
                      return coerce3($284.type);
                    };
                    var mkFieldRep = function(variant) {
                      return function(value14) {
                        return {
                          type: coerce3(fieldsKey(variant)),
                          value: value14
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
                      return bind3(bindFlipped5(function() {
                        var $285 = request2($$Proxy.value)(unit);
                        var $286 = getField(fieldKey);
                        return function($287) {
                          return $285(Validate.create($286($287)));
                        };
                      }())(gets2(function(v) {
                        return v.fieldObject;
                      })))(function(mbResult) {
                        return for_1(mbResult)(function(resultVariant) {
                          return modify_3(function(state3) {
                            var fieldObject = setFieldResult(resultVariant)(state3.fieldObject);
                            var $199 = {};
                            for (var $200 in state3) {
                              if ({}.hasOwnProperty.call(state3, $200)) {
                                $199[$200] = state3[$200];
                              }
                              ;
                            }
                            ;
                            $199.fieldObject = fieldObject;
                            $199.formState = function() {
                              var $196 = {};
                              for (var $197 in state3.formState) {
                                if ({}.hasOwnProperty.call(state3.formState, $197)) {
                                  $196[$197] = state3["formState"][$197];
                                }
                                ;
                              }
                              ;
                              $196.errorCount = countErrors(fieldObject);
                              $196.allTouched = allTouched(fieldObject);
                              return $196;
                            }();
                            return $199;
                          });
                        });
                      });
                    };
                    var runFormAction = function(action2) {
                      return handleAction(new HandleForm(new Eval(action2)));
                    };
                    var handleAction = function(v) {
                      if (v instanceof Initialize2) {
                        return bind3(get3)(function(v1) {
                          return when$prime1(v1.formConfig.validateOnMount)(function(v2) {
                            return for_22(getKeys(v1.fieldObject))(runValidation);
                          });
                        });
                      }
                      ;
                      if (v instanceof Receive2) {
                        return bind3(get3)(function(v1) {
                          return when$prime1(!unsafeRefEq(v1.input)(v.value0))(function(v2) {
                            return modify_3(function(v3) {
                              var $207 = {};
                              for (var $208 in v3) {
                                if ({}.hasOwnProperty.call(v3, $208)) {
                                  $207[$208] = v3[$208];
                                }
                                ;
                              }
                              ;
                              $207.input = v.value0;
                              return $207;
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
                          return discard2(for_1(v.value0.value0.value0)(function(event) {
                            return liftEffect9(preventDefault(event));
                          }))(function() {
                            return discard2(bind3(get3)(function(v1) {
                              return for_22(getKeys(v1.fieldObject))(runValidation);
                            }))(function() {
                              return bind3(get3)(function(v1) {
                                return discard2(modify_3(function(state3) {
                                  var $221 = {};
                                  for (var $222 in state3) {
                                    if ({}.hasOwnProperty.call(state3, $222)) {
                                      $221[$222] = state3[$222];
                                    }
                                    ;
                                  }
                                  ;
                                  $221.formState = function() {
                                    var $218 = {};
                                    for (var $219 in state3.formState) {
                                      if ({}.hasOwnProperty.call(state3.formState, $219)) {
                                        $218[$219] = state3["formState"][$219];
                                      }
                                      ;
                                    }
                                    ;
                                    $218.submitCount = state3.formState.submitCount + 1 | 0;
                                    return $218;
                                  }();
                                  return $221;
                                }))(function() {
                                  return for_1(mkFieldResults2(v1.fieldObject))(function(results) {
                                    var v2 = mkFieldOutputs2(results);
                                    if (v2 instanceof Nothing) {
                                      return tell3($$Proxy.value)(unit)(SubmitAttempt.create(results));
                                    }
                                    ;
                                    if (v2 instanceof Just) {
                                      return tell3($$Proxy.value)(unit)(Submit.create(v2.value0));
                                    }
                                    ;
                                    throw new Error("Failed pattern match at Formless (line 425, column 75 - line 427, column 64): " + [v2.constructor.name]);
                                  });
                                });
                              });
                            });
                          });
                        }
                        ;
                        if (v.value0.value0 instanceof SetForm) {
                          return modify_3(function(state3) {
                            var $228 = {};
                            for (var $229 in state3) {
                              if ({}.hasOwnProperty.call(state3, $229)) {
                                $228[$229] = state3[$229];
                              }
                              ;
                            }
                            ;
                            $228.fieldObject = v.value0.value0.value0;
                            return $228;
                          });
                        }
                        ;
                        if (v.value0.value0 instanceof SetFormConfig) {
                          return modify_3(function(state3) {
                            var $232 = {};
                            for (var $233 in state3) {
                              if ({}.hasOwnProperty.call(state3, $233)) {
                                $232[$233] = state3[$233];
                              }
                              ;
                            }
                            ;
                            $232.formConfig = v.value0.value0.value0;
                            return $232;
                          });
                        }
                        ;
                        if (v.value0.value0 instanceof ResetForm) {
                          var reset2 = function(field) {
                            var $236 = {};
                            for (var $237 in field) {
                              if ({}.hasOwnProperty.call(field, $237)) {
                                $236[$237] = field[$237];
                              }
                              ;
                            }
                            ;
                            $236.value = field.initialValue;
                            $236.result = Nothing.value;
                            return $236;
                          };
                          return discard2(modify_3(function(state3) {
                            var $239 = {};
                            for (var $240 in state3) {
                              if ({}.hasOwnProperty.call(state3, $240)) {
                                $239[$240] = state3[$240];
                              }
                              ;
                            }
                            ;
                            $239.fieldObject = map13(reset2)(state3.fieldObject);
                            $239.formState = {
                              submitCount: 0,
                              errorCount: 0,
                              allTouched: false
                            };
                            return $239;
                          }))(function() {
                            return tell3($$Proxy.value)(unit)(Reset.create);
                          });
                        }
                        ;
                        if (v.value0.value0 instanceof ChangeField) {
                          return bind3(get3)(function(v1) {
                            var modify9 = mkFieldRep(v.value0.value0.value0)(function(v2) {
                              return {
                                value: v.value0.value0.value1,
                                initialValue: v2.initialValue,
                                result: v2.result
                              };
                            });
                            return discard2(modify_3(function(state3) {
                              var $243 = {};
                              for (var $244 in state3) {
                                if ({}.hasOwnProperty.call(state3, $244)) {
                                  $243[$244] = state3[$244];
                                }
                                ;
                              }
                              ;
                              $243.fieldObject = modifyField(modify9)(state3.fieldObject);
                              return $243;
                            }))(function() {
                              return when$prime1(v1.formConfig.validateOnChange)(function(v2) {
                                return runFormAction(new ValidateField(v.value0.value0.value0));
                              });
                            });
                          });
                        }
                        ;
                        if (v.value0.value0 instanceof BlurField) {
                          return bind3(get3)(function(v1) {
                            return when$prime1(v1.formConfig.validateOnBlur)(function(v2) {
                              return runFormAction(new ValidateField(v.value0.value0.value0));
                            });
                          });
                        }
                        ;
                        if (v.value0.value0 instanceof ModifyField) {
                          return bind3(get3)(function(v1) {
                            var modify9 = mkFieldRep(v.value0.value0.value0)(v.value0.value0.value1);
                            return discard2(modify_3(function(state3) {
                              var $254 = {};
                              for (var $255 in state3) {
                                if ({}.hasOwnProperty.call(state3, $255)) {
                                  $254[$255] = state3[$255];
                                }
                                ;
                              }
                              ;
                              $254.fieldObject = modifyField(modify9)(state3.fieldObject);
                              return $254;
                            }))(function() {
                              return when$prime1(v1.formConfig.validateOnModify)(function(v2) {
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
                          var modify8 = mkFieldRep(v.value0.value0.value0)(reset2);
                          return modify_3(function(state3) {
                            var $261 = {};
                            for (var $262 in state3) {
                              if ({}.hasOwnProperty.call(state3, $262)) {
                                $261[$262] = state3[$262];
                              }
                              ;
                            }
                            ;
                            $261.fieldObject = modifyField(modify8)(state3.fieldObject);
                            return $261;
                          });
                        }
                        ;
                        throw new Error("Failed pattern match at Formless (line 417, column 33 - line 469, column 89): " + [v.value0.value0.constructor.name]);
                      }
                      ;
                      throw new Error("Failed pattern match at Formless (line 403, column 18 - line 469, column 89): " + [v.constructor.name]);
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
                        return slot2($$Proxy.value)(unit)(component)(context)(HandleForm.create);
                      },
                      "eval": mkEval({
                        initialize: new Just(Initialize2.value),
                        receive: function($288) {
                          return Just.create(Receive2.create($288));
                        },
                        finalize: Nothing.value,
                        handleAction,
                        handleQuery: function() {
                          var $289 = query3($$Proxy.value)(unit);
                          return function($290) {
                            return $289(Query2.create($290));
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
  var $$eval = function($291) {
    return raise(Eval.create($291));
  };
  var defaultConfig = {
    validateOnBlur: true,
    validateOnChange: false,
    validateOnModify: false,
    validateOnMount: false
  };
  var mkConfig1 = function(dictDefaults) {
    var defaults2 = defaults(dictDefaults);
    return {
      mkConfig: function(provided) {
        return defaults2(defaultConfig)(provided);
      },
      Defaults0: function() {
        return dictDefaults;
      }
    };
  };

  // output/Control.Monad.Except/index.js
  var unwrap3 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap3(runExceptT($3));
  };

  // output/Foreign.Index/foreign.js
  function unsafeReadPropImpl(f, s, key, value14) {
    return value14 == null ? f : s(value14[key]);
  }

  // output/Foreign.Index/index.js
  var unsafeReadProp = function(dictMonad) {
    var fail2 = fail(dictMonad);
    var pure16 = pure(applicativeExceptT(dictMonad));
    return function(k) {
      return function(value14) {
        return unsafeReadPropImpl(fail2(new TypeMismatch("object", typeOf(value14))), pure16, k, value14);
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
  var map14 = /* @__PURE__ */ map(functorMaybe);
  var item = function(i2) {
    var $5 = _item(i2);
    return function($6) {
      return toMaybe($5($6));
    };
  };
  var items = function(dictUnfoldable) {
    var unfoldr3 = unfoldr(dictUnfoldable);
    return function(fl) {
      return unfoldr3(function(i2) {
        return map14(flip(Tuple.create)(i2 + 1 | 0))(item(i2)(fl));
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
  var map15 = /* @__PURE__ */ map(functorEffect);
  var fromEventTarget = /* @__PURE__ */ unsafeReadProtoTagged("HTMLInputElement");
  var files = /* @__PURE__ */ function() {
    var $6 = map15(toMaybe);
    return function($7) {
      return $6(_files($7));
    };
  }();

  // output/Halogen.HTML.Events/index.js
  var map16 = /* @__PURE__ */ map(functorMaybe);
  var composeKleisli2 = /* @__PURE__ */ composeKleisli(bindMaybe);
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var readProp2 = /* @__PURE__ */ readProp(monadIdentity);
  var readString2 = /* @__PURE__ */ readString(monadIdentity);
  var handler$prime = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return map16(Action.create)(f(ev));
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
    var none2 = none(dictUnfoldable);
    var items2 = items(dictUnfoldable);
    return function(f) {
      return handler2(change)(function() {
        var $19 = maybe(none2)(items2);
        var $20 = composeKleisli2(target)(composeKleisli2(fromEventTarget)(function($22) {
          return unsafePerformEffect(files($22));
        }));
        return function($21) {
          return f($19($20($21)));
        };
      }());
    };
  };
  var onSubmit = /* @__PURE__ */ handler2("submit");
  var focusHandler = unsafeCoerce2;
  var onBlur = /* @__PURE__ */ function() {
    var $55 = handler2(blur2);
    return function($56) {
      return $55(focusHandler($56));
    };
  }();
  var addForeignPropHandler = function(key) {
    return function(prop3) {
      return function(reader) {
        return function(f) {
          var go2 = function(a2) {
            return composeKleisliFlipped2(reader)(readProp2(prop3))(unsafeToForeign(a2));
          };
          return handler$prime(key)(composeKleisli2(currentTarget)(function(e) {
            return either($$const(Nothing.value))(function($85) {
              return Just.create(f($85));
            })(runExcept(go2(e)));
          }));
        };
      };
    };
  };
  var onChecked = /* @__PURE__ */ addForeignPropHandler(change)("checked")(/* @__PURE__ */ readBoolean(monadIdentity));
  var onValueInput = /* @__PURE__ */ addForeignPropHandler(input2)("value")(readString2);

  // output/Example.Basic/index.js
  var type_4 = /* @__PURE__ */ type_(isPropInputType);
  var type_1 = /* @__PURE__ */ type_(isPropButtonType);
  var put2 = /* @__PURE__ */ put(monadStateHalogenM);
  var messageIsSymbol = {
    reflectSymbol: function() {
      return "message";
    }
  };
  var mapRecordWithIndexCons2 = /* @__PURE__ */ mapRecordWithIndexCons(messageIsSymbol);
  var constMapping2 = /* @__PURE__ */ constMapping(mappingMkFieldStateFieldS);
  var nameIsSymbol = {
    reflectSymbol: function() {
      return "name";
    }
  };
  var mapRecordWithIndexCons1 = /* @__PURE__ */ mapRecordWithIndexCons(nameIsSymbol);
  var hfoldlRecordWithIndex2 = /* @__PURE__ */ hfoldlRecordWithIndex();
  var foldlRecordCons2 = /* @__PURE__ */ foldlRecordCons(messageIsSymbol)();
  var foldlRecordCons1 = /* @__PURE__ */ foldlRecordCons(nameIsSymbol)();
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
      return form([onSubmit(v.formActions.handleSubmit)])([div_([label_([text("Name")]), input([type_4(InputText.value), onValueInput(v.actions.name.handleChange), onBlur(v.actions.name.handleBlur), function() {
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
      }()]), div_([label_([text("Message")]), textarea([onValueInput(v.actions.message.handleChange), onBlur(v.actions.message.handleBlur)])]), button([type_1(ButtonSubmit.value)])([text("Submit")])]);
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
        return put2(v.value0);
      }
      ;
      if (v instanceof Eval2) {
        return $$eval(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Example.Basic (line 42, column 18 - line 44, column 33): " + [v.constructor.name]);
    };
    return formless(monadEffectAff)(mkFieldStates1(hmapRecord()(mapRecordWithIndexCons2(constMapping2)(mapRecordWithIndexCons1(constMapping2)(mapRecordWithIndexNil)()())()())))(mkFieldActions1(hmapWithIndexRecord()(mapRecordWithIndexCons2(mappingWithIndexMkFieldAc(messageIsSymbol)(refl)())(mapRecordWithIndexCons1(mappingWithIndexMkFieldAc(nameIsSymbol)(refl)())(mapRecordWithIndexNil)()())()())))(mkFieldResults1(hfoldlRecordWithIndex2(foldlRecordCons2(foldingWithIndexMkFieldRe(messageIsSymbol)(refl)()())(foldlRecordCons1(foldingWithIndexMkFieldRe(nameIsSymbol)(refl)()())(foldlRecordNil)))))(mkFieldOutputs1(hfoldlRecordWithIndex2(foldlRecordCons2(foldingWithIndexMkFieldOu(messageIsSymbol)(refl)()())(foldlRecordCons1(foldingWithIndexMkFieldOu(nameIsSymbol)(refl)()())(foldlRecordNil)))))(mkConfig1(defaultsRecord()()))({
      liftAction: Eval2.create
    })(mempty(monoidRecord()(monoidRecordCons(messageIsSymbol)(monoidString)()(monoidRecordCons(nameIsSymbol)(monoidString)()(monoidRecordNil)))))(mkComponent({
      initialState: identity(categoryFn),
      render: render3,
      "eval": mkEval({
        handleAction,
        handleQuery: handleQuery2,
        receive: function($115) {
          return Just.create(Receive3.create($115));
        },
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize
      })
    }));
  }();

  // output/Example.Utils.Field/index.js
  var append5 = /* @__PURE__ */ append(semigroupArray);
  var value3 = /* @__PURE__ */ value(isPropString);
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorArray);
  var type_5 = /* @__PURE__ */ type_(isPropInputType);
  var onFileUpload2 = /* @__PURE__ */ onFileUpload(unfoldableArray);
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
    var $64 = withLabel({
      label: v.label,
      state: v.state
    });
    var $65 = append5([value3(v.state.value), function() {
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
    return function($66) {
      return $64(input($65($66)));
    };
  };
  var radioGroup = function(dictEq) {
    var eq3 = eq(dictEq);
    return function(v) {
      return div_([label_([text(v.label)]), fieldset_(mapFlipped2(v.options)(function(v1) {
        return label_([input(flip(append5)(v1.props)([type_5(InputRadio.value), name2(v.action.key), checked(eq3(v.state.value)(v1.option)), onChange(function(v2) {
          return v.action.handleChange(v1.option);
        }), onBlur(v.action.handleBlur)])), text(v1.render)]);
      }))]);
    };
  };
  var fileUpload = function(v) {
    return function(props) {
      return div_([label([$$for(v.action.key)])([text(v.label), input(flip(append5)(props)([name2(v.action.key), type_5(InputFile.value), onFileUpload2(v.action.handleChange), onBlur(v.action.handleBlur)]))]), function() {
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
      return fieldset_([label_([input(flip(append5)(props)([type_5(InputCheckbox.value), checked(v.state.value), onChecked(v.action.handleChange), onBlur(v.action.handleBlur)])), text(v.label)])]);
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
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map17 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var div3 = /* @__PURE__ */ div(euclideanRingInt);
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
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
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $43 = isLead(cu0) && isTrail(cu1);
    if ($43) {
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
    return map17(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons5(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $47 = isLead(cu0) && length3(s) > 1;
    if ($47) {
      var cu1 = fromEnum2(charAt(1)(s));
      var $48 = isTrail(cu1);
      if ($48) {
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
  var length6 = function($74) {
    return length(toCodePointArray($74));
  };
  var indexOf2 = function(p2) {
    return function(s) {
      return map17(function(i2) {
        return length6(take2(i2)(s));
      })(indexOf(p2)(s));
    };
  };
  var fromCharCode2 = /* @__PURE__ */ function() {
    var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($76) {
      return singleton6($75($76));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div3(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode2(lead) + fromCharCode2(trail);
  };
  var singleton7 = /* @__PURE__ */ _singleton(singletonFallback);
  var takeFallback = function(v) {
    return function(v1) {
      if (v < 1) {
        return "";
      }
      ;
      var v2 = uncons5(v1);
      if (v2 instanceof Just) {
        return singleton7(v2.value0.head) + takeFallback(v - 1 | 0)(v2.value0.tail);
      }
      ;
      return v1;
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
            var $61 = p2(v.value0.head);
            if ($61) {
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
  var codePointFromChar = function($77) {
    return CodePoint(fromEnum2($77));
  };

  // output/Example.Utils.Validation/index.js
  var show2 = /* @__PURE__ */ show(showInt);
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
        return new Left("Must be longer than " + (show2(limit) + " characters."));
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
    var $15 = map(functorEither)(Username);
    var $16 = composeKleisliFlipped(bindEither)(longerThan(5))(requiredText);
    return function($17) {
      return $15($16($17));
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
  var radioGroup2 = /* @__PURE__ */ radioGroup(eqPicked);
  var type_6 = /* @__PURE__ */ type_(isPropButtonType);
  var put3 = /* @__PURE__ */ put(monadStateHalogenM);
  var nameIsSymbol2 = {
    reflectSymbol: function() {
      return "name";
    }
  };
  var mapRecordWithIndexCons3 = /* @__PURE__ */ mapRecordWithIndexCons(nameIsSymbol2);
  var constMapping3 = /* @__PURE__ */ constMapping(mappingMkFieldStateFieldS);
  var pickedIsSymbol = {
    reflectSymbol: function() {
      return "picked";
    }
  };
  var mapRecordWithIndexCons12 = /* @__PURE__ */ mapRecordWithIndexCons(pickedIsSymbol);
  var subscribeIsSymbol = {
    reflectSymbol: function() {
      return "subscribe";
    }
  };
  var mapRecordWithIndexCons22 = /* @__PURE__ */ mapRecordWithIndexCons(subscribeIsSymbol);
  var hfoldlRecordWithIndex3 = /* @__PURE__ */ hfoldlRecordWithIndex();
  var foldlRecordCons3 = /* @__PURE__ */ foldlRecordCons(nameIsSymbol2)();
  var foldlRecordCons12 = /* @__PURE__ */ foldlRecordCons(pickedIsSymbol)();
  var foldlRecordCons22 = /* @__PURE__ */ foldlRecordCons(subscribeIsSymbol)();
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
      }), radioGroup2({
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
      }), br_, button([type_6(ButtonSubmit.value)])([text("Submit")])]);
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
        return put3(v.value0);
      }
      ;
      if (v instanceof Eval3) {
        return $$eval(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Example.CheckboxRadio (line 46, column 18 - line 48, column 33): " + [v.constructor.name]);
    };
    return formless(monadEffectAff)(mkFieldStates1(hmapRecord()(mapRecordWithIndexCons3(constMapping3)(mapRecordWithIndexCons12(constMapping3)(mapRecordWithIndexCons22(constMapping3)(mapRecordWithIndexNil)()())()())()())))(mkFieldActions1(hmapWithIndexRecord()(mapRecordWithIndexCons3(mappingWithIndexMkFieldAc(nameIsSymbol2)(refl)())(mapRecordWithIndexCons12(mappingWithIndexMkFieldAc(pickedIsSymbol)(refl)())(mapRecordWithIndexCons22(mappingWithIndexMkFieldAc(subscribeIsSymbol)(refl)())(mapRecordWithIndexNil)()())()())()())))(mkFieldResults1(hfoldlRecordWithIndex3(foldlRecordCons3(foldingWithIndexMkFieldRe(nameIsSymbol2)(refl)()())(foldlRecordCons12(foldingWithIndexMkFieldRe(pickedIsSymbol)(refl)()())(foldlRecordCons22(foldingWithIndexMkFieldRe(subscribeIsSymbol)(refl)()())(foldlRecordNil))))))(mkFieldOutputs1(hfoldlRecordWithIndex3(foldlRecordCons3(foldingWithIndexMkFieldOu(nameIsSymbol2)(refl)()())(foldlRecordCons12(foldingWithIndexMkFieldOu(pickedIsSymbol)(refl)()())(foldlRecordCons22(foldingWithIndexMkFieldOu(subscribeIsSymbol)(refl)()())(foldlRecordNil))))))(mkConfig1(defaultsRecord()()))({
      liftAction: Eval3.create
    })(initialForm)(mkComponent({
      initialState: identity(categoryFn),
      render: render3,
      "eval": mkEval({
        handleAction,
        handleQuery: handleQuery2,
        receive: function($121) {
          return Just.create(Receive4.create($121));
        },
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize
      })
    }));
  }();

  // output/Example.DependentFields/index.js
  var type_7 = /* @__PURE__ */ type_(isPropInputType);
  var type_12 = /* @__PURE__ */ type_(isPropButtonType);
  var put4 = /* @__PURE__ */ put(monadStateHalogenM);
  var bindExceptT2 = /* @__PURE__ */ bindExceptT(monadHalogenM);
  var bind4 = /* @__PURE__ */ bind(bindExceptT2);
  var except2 = /* @__PURE__ */ except(applicativeHalogenM);
  var bindFlipped6 = /* @__PURE__ */ bindFlipped(bindEither);
  var discard3 = /* @__PURE__ */ discard(discardUnit);
  var discard1 = /* @__PURE__ */ discard3(bindExceptT2);
  var lift3 = /* @__PURE__ */ lift(monadTransExceptT)(monadHalogenM);
  var $$void7 = /* @__PURE__ */ $$void(functorHalogenM);
  var discard22 = /* @__PURE__ */ discard3(bindHalogenM);
  var liftAff2 = /* @__PURE__ */ liftAff(/* @__PURE__ */ monadAffHalogenM(monadAffAff));
  var pure6 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeExceptT(monadHalogenM));
  var bind12 = /* @__PURE__ */ bind(bindHalogenM);
  var gets3 = /* @__PURE__ */ gets(monadStateHalogenM);
  var when2 = /* @__PURE__ */ when(applicativeHalogenM);
  var notEq2 = /* @__PURE__ */ notEq(eqCodePoint);
  var pure12 = /* @__PURE__ */ pure(applicativeHalogenM);
  var emailIsSymbol = {
    reflectSymbol: function() {
      return "email";
    }
  };
  var mapRecordWithIndexCons4 = /* @__PURE__ */ mapRecordWithIndexCons(emailIsSymbol);
  var constMapping4 = /* @__PURE__ */ constMapping(mappingMkFieldStateFieldS);
  var password1IsSymbol = {
    reflectSymbol: function() {
      return "password1";
    }
  };
  var mapRecordWithIndexCons13 = /* @__PURE__ */ mapRecordWithIndexCons(password1IsSymbol);
  var password2IsSymbol = {
    reflectSymbol: function() {
      return "password2";
    }
  };
  var mapRecordWithIndexCons23 = /* @__PURE__ */ mapRecordWithIndexCons(password2IsSymbol);
  var usernameIsSymbol = {
    reflectSymbol: function() {
      return "username";
    }
  };
  var mapRecordWithIndexCons32 = /* @__PURE__ */ mapRecordWithIndexCons(usernameIsSymbol);
  var hfoldlRecordWithIndex4 = /* @__PURE__ */ hfoldlRecordWithIndex();
  var foldlRecordCons4 = /* @__PURE__ */ foldlRecordCons(emailIsSymbol)();
  var foldlRecordCons13 = /* @__PURE__ */ foldlRecordCons(password1IsSymbol)();
  var foldlRecordCons23 = /* @__PURE__ */ foldlRecordCons(password2IsSymbol)();
  var foldlRecordCons32 = /* @__PURE__ */ foldlRecordCons(usernameIsSymbol)();
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
      })([type_7(InputPassword.value)]), textInput({
        label: "Password (Confirm)",
        state: v.fields.password2,
        action: v.actions.password2
      })([type_7(InputPassword.value)]), button([type_12(ButtonSubmit.value)])([text("Submit")])]);
    };
    var handleAction = function(v) {
      if (v instanceof Receive5) {
        return put4(v.value0);
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
            return bind4(except2(bindFlipped6(longerThan(3))(requiredText(input3))))(function(password) {
              return bind4(except2(validateEq(password)(otherPassword.value)))(function() {
                return discard1(function() {
                  if (otherPassword.result instanceof Just && otherPassword.result.value0 instanceof Left) {
                    return lift3($$void7(fork(discard22(liftAff2(delay(10)))(function() {
                      return handleAction(otherPassword.validate);
                    }))));
                  }
                  ;
                  return pure6(unit);
                }())(function() {
                  return pure6(password);
                });
              });
            });
          }());
        };
      };
      var validation2 = {
        email: function(input3) {
          var validated = email(input3);
          return bind12(gets3(function(v) {
            return v.fields.username.result;
          }))(function(usernameResult) {
            return discard22(when2(isRight(validated) && isNothing(usernameResult))(function() {
              var start2 = takeWhile2(function(v) {
                return notEq2(v)(codePointFromChar("@"));
              })(input3);
              return bind12(gets3(function(v) {
                return v.actions.username.modify;
              }))(function(modifyUsername) {
                return handleAction(modifyUsername(function(v) {
                  var $207 = {};
                  for (var $208 in v) {
                    if ({}.hasOwnProperty.call(v, $208)) {
                      $207[$208] = v[$208];
                    }
                    ;
                  }
                  ;
                  $207.value = start2;
                  return $207;
                }));
              });
            }()))(function() {
              return pure12(validated);
            });
          });
        },
        username: function($224) {
          return pure12(username($224));
        },
        password1: function(input3) {
          return bind12(gets3(function(v) {
            return v.fields.password2;
          }))(function(v) {
            return bind12(gets3(function(v1) {
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
          return bind12(gets3(function(v) {
            return v.fields.password1;
          }))(function(v) {
            return bind12(gets3(function(v1) {
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
      return handleSubmitValidateM(function($225) {
        return raise2(toOutput($225));
      })(validateM()()()()(applicativeHalogenM))(validation2);
    }();
    return formless(monadEffectAff)(mkFieldStates1(hmapRecord()(mapRecordWithIndexCons4(constMapping4)(mapRecordWithIndexCons13(constMapping4)(mapRecordWithIndexCons23(constMapping4)(mapRecordWithIndexCons32(constMapping4)(mapRecordWithIndexNil)()())()())()())()())))(mkFieldActions1(hmapWithIndexRecord()(mapRecordWithIndexCons4(mappingWithIndexMkFieldAc(emailIsSymbol)(refl)())(mapRecordWithIndexCons13(mappingWithIndexMkFieldAc(password1IsSymbol)(refl)())(mapRecordWithIndexCons23(mappingWithIndexMkFieldAc(password2IsSymbol)(refl)())(mapRecordWithIndexCons32(mappingWithIndexMkFieldAc(usernameIsSymbol)(refl)())(mapRecordWithIndexNil)()())()())()())()())))(mkFieldResults1(hfoldlRecordWithIndex4(foldlRecordCons4(foldingWithIndexMkFieldRe(emailIsSymbol)(refl)()())(foldlRecordCons13(foldingWithIndexMkFieldRe(password1IsSymbol)(refl)()())(foldlRecordCons23(foldingWithIndexMkFieldRe(password2IsSymbol)(refl)()())(foldlRecordCons32(foldingWithIndexMkFieldRe(usernameIsSymbol)(refl)()())(foldlRecordNil)))))))(mkFieldOutputs1(hfoldlRecordWithIndex4(foldlRecordCons4(foldingWithIndexMkFieldOu(emailIsSymbol)(refl)()())(foldlRecordCons13(foldingWithIndexMkFieldOu(password1IsSymbol)(refl)()())(foldlRecordCons23(foldingWithIndexMkFieldOu(password2IsSymbol)(refl)()())(foldlRecordCons32(foldingWithIndexMkFieldOu(usernameIsSymbol)(refl)()())(foldlRecordNil)))))))(mkConfig1(defaultsRecord()()))({
      liftAction: Eval4.create,
      validateOnModify: true
    })(mempty(monoidRecord()(monoidRecordCons(emailIsSymbol)(monoidString)()(monoidRecordCons(password1IsSymbol)(monoidString)()(monoidRecordCons(password2IsSymbol)(monoidString)()(monoidRecordCons(usernameIsSymbol)(monoidString)()(monoidRecordNil)))))))(mkComponent({
      initialState: identity(categoryFn),
      render: render3,
      "eval": mkEval({
        handleAction,
        handleQuery: handleQuery2,
        receive: function($226) {
          return Just.create(Receive5.create($226));
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
  var type_8 = function(blob) {
    var blobType = typeImpl(blob);
    var $2 = blobType === "";
    if ($2) {
      return Nothing.value;
    }
    ;
    return new Just(blobType);
  };

  // output/Web.File.File/index.js
  var type_9 = function($1) {
    return type_8($1);
  };
  var size4 = function($2) {
    return size3($2);
  };

  // output/Example.FileUpload/index.js
  var elem3 = /* @__PURE__ */ elem2(eqMediaType);
  var unwrap4 = /* @__PURE__ */ unwrap();
  var show3 = /* @__PURE__ */ show(showNumber);
  var type_10 = /* @__PURE__ */ type_(isPropButtonType);
  var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(bindEither);
  var put5 = /* @__PURE__ */ put(monadStateHalogenM);
  var nameIsSymbol3 = {
    reflectSymbol: function() {
      return "name";
    }
  };
  var mapRecordWithIndexCons5 = /* @__PURE__ */ mapRecordWithIndexCons(nameIsSymbol3);
  var constMapping5 = /* @__PURE__ */ constMapping(mappingMkFieldStateFieldS);
  var photoIsSymbol = {
    reflectSymbol: function() {
      return "photo";
    }
  };
  var mapRecordWithIndexCons14 = /* @__PURE__ */ mapRecordWithIndexCons(photoIsSymbol);
  var hfoldlRecordWithIndex5 = /* @__PURE__ */ hfoldlRecordWithIndex();
  var foldlRecordCons5 = /* @__PURE__ */ foldlRecordCons(nameIsSymbol3)();
  var foldlRecordCons14 = /* @__PURE__ */ foldlRecordCons(photoIsSymbol)();
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
    var v = type_9(file);
    if (v instanceof Nothing) {
      return new Left("Unrecognized file type. Accepted types: png, jpeg, gif.");
    }
    ;
    if (v instanceof Just && elem3(v.value0)([imagePNG, imageJPEG, imageGIF])) {
      return new Right(file);
    }
    ;
    if (v instanceof Just) {
      return new Left("Unsupported file type: " + (unwrap4(v.value0) + ". Accepted types: png, jpeg, gif."));
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
          return small_([text("Your photo size: " + show3(v1.size))]);
        }
      }), br_, button([type_10(ButtonSubmit.value)])([text("Submit")])]);
    };
    var handleQuery2 = handleSubmitValidate(raise2)(validate()()()())({
      name: requiredText,
      photo: composeKleisliFlipped3(validateFileSize)(composeKleisliFlipped3(validateFileType)(validateFileCount))
    });
    var handleAction = function(v) {
      if (v instanceof Receive6) {
        return put5(v.value0);
      }
      ;
      if (v instanceof Eval5) {
        return $$eval(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Example.FileUpload (line 47, column 18 - line 49, column 33): " + [v.constructor.name]);
    };
    return formless(monadEffectAff)(mkFieldStates1(hmapRecord()(mapRecordWithIndexCons5(constMapping5)(mapRecordWithIndexCons14(constMapping5)(mapRecordWithIndexNil)()())()())))(mkFieldActions1(hmapWithIndexRecord()(mapRecordWithIndexCons5(mappingWithIndexMkFieldAc(nameIsSymbol3)(refl)())(mapRecordWithIndexCons14(mappingWithIndexMkFieldAc(photoIsSymbol)(refl)())(mapRecordWithIndexNil)()())()())))(mkFieldResults1(hfoldlRecordWithIndex5(foldlRecordCons5(foldingWithIndexMkFieldRe(nameIsSymbol3)(refl)()())(foldlRecordCons14(foldingWithIndexMkFieldRe(photoIsSymbol)(refl)()())(foldlRecordNil)))))(mkFieldOutputs1(hfoldlRecordWithIndex5(foldlRecordCons5(foldingWithIndexMkFieldOu(nameIsSymbol3)(refl)()())(foldlRecordCons14(foldingWithIndexMkFieldOu(photoIsSymbol)(refl)()())(foldlRecordNil)))))(mkConfig1(defaultsRecord()()))({
      liftAction: Eval5.create
    })(mempty(monoidRecord()(monoidRecordCons(nameIsSymbol3)(monoidString)()(monoidRecordCons(photoIsSymbol)(monoidArray)()(monoidRecordNil)))))(mkComponent({
      initialState: identity(categoryFn),
      render: render3,
      "eval": mkEval({
        handleAction,
        handleQuery: handleQuery2,
        receive: function($126) {
          return Just.create(Receive6.create($126));
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
  var show1 = /* @__PURE__ */ show(showInt);
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
        return "  At array index " + (show1(v.value0) + (":\n" + go2(v.value1)));
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
    return function(apply4) {
      return function(map29) {
        return function(f) {
          var buildFrom = function(x, ys) {
            return apply4(map29(consList)(f(x)))(ys);
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
            var acc = map29(finalCell)(f(array[array.length - 1]));
            var result = go2(acc, array.length - 1, array);
            while (result instanceof Cont) {
              result = result.fn();
            }
            return map29(listToArray)(result);
          };
        };
      };
    };
  }();

  // output/Data.Argonaut.Decode.Decoders/index.js
  var pure7 = /* @__PURE__ */ pure(applicativeEither);
  var map18 = /* @__PURE__ */ map(functorEither);
  var lmap2 = /* @__PURE__ */ lmap(bifunctorEither);
  var bind5 = /* @__PURE__ */ bind(bindEither);
  var decodeString = /* @__PURE__ */ function() {
    return caseJsonString(new Left(new TypeMismatch2("String")))(Right.create);
  }();
  var decodeMaybe = function(decoder) {
    return function(json) {
      if (isNull2(json)) {
        return pure7(Nothing.value);
      }
      ;
      if (otherwise) {
        return map18(Just.create)(decoder(json));
      }
      ;
      throw new Error("Failed pattern match at Data.Argonaut.Decode.Decoders (line 37, column 1 - line 41, column 38): " + [decoder.constructor.name, json.constructor.name]);
    };
  };
  var decodeJObject = /* @__PURE__ */ function() {
    var $50 = note(new TypeMismatch2("Object"));
    return function($51) {
      return $50(toObject($51));
    };
  }();
  var decodeEither = function(decoderA) {
    return function(decoderB) {
      return function(json) {
        return lmap2(Named.create("Either"))(bind5(decodeJObject(json))(function(obj) {
          return bind5(note(new AtKey("tag", MissingValue.value))(lookup2("tag")(obj)))(function(tag) {
            return bind5(note(new AtKey("value", MissingValue.value))(lookup2("value")(obj)))(function(val) {
              var v = toString(tag);
              if (v instanceof Just && v.value0 === "Right") {
                return map18(Right.create)(decoderB(val));
              }
              ;
              if (v instanceof Just && v.value0 === "Left") {
                return map18(Left.create)(decoderA(val));
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
  var bind6 = /* @__PURE__ */ bind(bindEither);
  var lmap3 = /* @__PURE__ */ lmap(bifunctorEither);
  var map19 = /* @__PURE__ */ map(functorMaybe);
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
    var gDecodeJson1 = gDecodeJson(dictGDecodeJson);
    return function() {
      return {
        decodeJson: function(json) {
          var v = toObject(json);
          if (v instanceof Just) {
            return gDecodeJson1(v.value0)($$Proxy.value);
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
    var decodeJsonField1 = decodeJsonField(dictDecodeJsonField);
    return function(dictGDecodeJson) {
      var gDecodeJson1 = gDecodeJson(dictGDecodeJson);
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        var insert10 = insert(dictIsSymbol)()();
        return function() {
          return function() {
            return {
              gDecodeJson: function(object2) {
                return function(v) {
                  var fieldName = reflectSymbol2($$Proxy.value);
                  var fieldValue = lookup2(fieldName)(object2);
                  var v1 = decodeJsonField1(fieldValue);
                  if (v1 instanceof Just) {
                    return bind6(lmap3(AtKey.create(fieldName))(v1.value0))(function(val) {
                      return bind6(gDecodeJson1(object2)($$Proxy.value))(function(rest) {
                        return new Right(insert10($$Proxy.value)(val)(rest));
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
    var decodeJson1 = decodeJson(dictDecodeJson);
    return function(dictDecodeJson1) {
      return {
        decodeJson: decodeEither(decodeJson1)(decodeJson(dictDecodeJson1))
      };
    };
  };
  var decodeJsonMaybe = function(dictDecodeJson) {
    return {
      decodeJson: decodeMaybe(decodeJson(dictDecodeJson))
    };
  };
  var decodeFieldMaybe = function(dictDecodeJson) {
    var decodeJson1 = decodeJson(decodeJsonMaybe(dictDecodeJson));
    return {
      decodeJsonField: function(v) {
        if (v instanceof Nothing) {
          return new Just(new Right(Nothing.value));
        }
        ;
        if (v instanceof Just) {
          return new Just(decodeJson1(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Argonaut.Decode.Class (line 139, column 1 - line 143, column 49): " + [v.constructor.name]);
      }
    };
  };
  var decodeFieldId = function(dictDecodeJson) {
    var decodeJson1 = decodeJson(dictDecodeJson);
    return {
      decodeJsonField: function(j) {
        return map19(decodeJson1)(j);
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
    var $3 = lmap(bifunctorEither)(function(v) {
      return new TypeMismatch2("JSON");
    });
    return function($4) {
      return $3(jsonParser($4));
    };
  }();

  // output/Data.Argonaut.Encode.Encoders/index.js
  var fromFoldable6 = /* @__PURE__ */ fromFoldable2(foldableList);
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
            return id2(fromFoldable6(new Cons(new Tuple("tag", id2(tag)), new Cons(new Tuple("value", encoder(x)), Nil.value))));
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
    var gEncodeJson1 = gEncodeJson(dictGEncodeJson);
    return function() {
      return {
        encodeJson: function(rec) {
          return id2(gEncodeJson1(rec)($$Proxy.value));
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
    var encodeJson1 = encodeJson(dictEncodeJson);
    return function(dictEncodeJson1) {
      return {
        encodeJson: encodeEither(encodeJson1)(encodeJson(dictEncodeJson1))
      };
    };
  };
  var encodeJsonMaybe = function(dictEncodeJson) {
    return {
      encodeJson: encodeMaybe(encodeJson(dictEncodeJson))
    };
  };
  var gEncodeJsonCons = function(dictEncodeJson) {
    var encodeJson1 = encodeJson(dictEncodeJson);
    return function(dictGEncodeJson) {
      var gEncodeJson1 = gEncodeJson(dictGEncodeJson);
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        var get4 = get2(dictIsSymbol)();
        return function() {
          return {
            gEncodeJson: function(row) {
              return function(v) {
                return insert3(reflectSymbol2($$Proxy.value))(encodeJson1(get4($$Proxy.value)(row)))(gEncodeJson1(row)($$Proxy.value));
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
    var $51 = liftEffect(dictMonadEffect);
    return function($52) {
      return $51(log2($52));
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
  var map20 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = /* @__PURE__ */ function() {
    var $2 = map20(function() {
      var $4 = fromMaybe(Loading.value);
      return function($5) {
        return $4(parse($5));
      };
    }());
    return function($3) {
      return $2(_readyState($3));
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
    return function(value14) {
      return function(storage) {
        return function() {
          storage.setItem(key, value14);
        };
      };
    };
  }

  // output/Web.Storage.Storage/index.js
  var map21 = /* @__PURE__ */ map(functorEffect);
  var getItem = function(s) {
    var $5 = map21(toMaybe);
    var $6 = _getItem(s);
    return function($7) {
      return $5($6($7));
    };
  };

  // output/Example.LocalStorage/index.js
  var type_27 = /* @__PURE__ */ type_(isPropButtonType);
  var bind7 = /* @__PURE__ */ bind(bindHalogenM);
  var monadEffectHalogenM2 = /* @__PURE__ */ monadEffectHalogenM(monadEffectAff);
  var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectHalogenM2);
  var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
  var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindEither);
  var gDecodeJsonCons2 = /* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(decodeJsonString));
  var valueIsSymbol = {
    reflectSymbol: function() {
      return "value";
    }
  };
  var resultIsSymbol = {
    reflectSymbol: function() {
      return "result";
    }
  };
  var initialValueIsSymbol = {
    reflectSymbol: function() {
      return "initialValue";
    }
  };
  var gDecodeJsonCons1 = /* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(/* @__PURE__ */ decodeRecord(/* @__PURE__ */ gDecodeJsonCons2(/* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldMaybe(/* @__PURE__ */ decodeJsonEither(decodeJsonString)(decodeJsonString)))(/* @__PURE__ */ gDecodeJsonCons2(gDecodeJsonNil)(valueIsSymbol)()())(resultIsSymbol)()())(initialValueIsSymbol)()())()));
  var stateIsSymbol = {
    reflectSymbol: function() {
      return "state";
    }
  };
  var nicknameIsSymbol = {
    reflectSymbol: function() {
      return "nickname";
    }
  };
  var nameIsSymbol4 = {
    reflectSymbol: function() {
      return "name";
    }
  };
  var cityIsSymbol = {
    reflectSymbol: function() {
      return "city";
    }
  };
  var decodeJson2 = /* @__PURE__ */ decodeJson(/* @__PURE__ */ decodeRecord(/* @__PURE__ */ gDecodeJsonCons1(/* @__PURE__ */ gDecodeJsonCons1(/* @__PURE__ */ gDecodeJsonCons1(/* @__PURE__ */ gDecodeJsonCons1(gDecodeJsonNil)(stateIsSymbol)()())(nicknameIsSymbol)()())(nameIsSymbol4)()())(cityIsSymbol)()())());
  var log4 = /* @__PURE__ */ log3(monadEffectHalogenM2);
  var gets4 = /* @__PURE__ */ gets(monadStateHalogenM);
  var gEncodeJsonCons2 = /* @__PURE__ */ gEncodeJsonCons(encodeJsonJString);
  var gEncodeJsonCons1 = /* @__PURE__ */ gEncodeJsonCons(/* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons2(/* @__PURE__ */ gEncodeJsonCons(/* @__PURE__ */ encodeJsonMaybe(/* @__PURE__ */ encodeJsonEither(encodeJsonJString)(encodeJsonJString)))(/* @__PURE__ */ gEncodeJsonCons2(gEncodeJsonNil)(valueIsSymbol)())(resultIsSymbol)())(initialValueIsSymbol)())());
  var encodeJson2 = /* @__PURE__ */ encodeJson(/* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons1(/* @__PURE__ */ gEncodeJsonCons1(/* @__PURE__ */ gEncodeJsonCons1(/* @__PURE__ */ gEncodeJsonCons1(gEncodeJsonNil)(stateIsSymbol)())(nicknameIsSymbol)())(nameIsSymbol4)())(cityIsSymbol)())());
  var discard4 = /* @__PURE__ */ discard(discardUnit)(bindHalogenM);
  var put6 = /* @__PURE__ */ put(monadStateHalogenM);
  var mapRecordWithIndexCons6 = /* @__PURE__ */ mapRecordWithIndexCons(cityIsSymbol);
  var constMapping6 = /* @__PURE__ */ constMapping(mappingMkFieldStateFieldS);
  var mapRecordWithIndexCons15 = /* @__PURE__ */ mapRecordWithIndexCons(nameIsSymbol4);
  var mapRecordWithIndexCons24 = /* @__PURE__ */ mapRecordWithIndexCons(nicknameIsSymbol);
  var mapRecordWithIndexCons33 = /* @__PURE__ */ mapRecordWithIndexCons(stateIsSymbol);
  var hfoldlRecordWithIndex6 = /* @__PURE__ */ hfoldlRecordWithIndex();
  var foldlRecordCons6 = /* @__PURE__ */ foldlRecordCons(cityIsSymbol)();
  var foldlRecordCons15 = /* @__PURE__ */ foldlRecordCons(nameIsSymbol4)();
  var foldlRecordCons24 = /* @__PURE__ */ foldlRecordCons(nicknameIsSymbol)();
  var foldlRecordCons33 = /* @__PURE__ */ foldlRecordCons(stateIsSymbol)();
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
      })([placeholder("California")]), br_, button([type_27(ButtonSubmit.value)])([text("Submit")])]);
    };
    var handleQuery2 = handleSubmitValidate(raise2)(validate()()()())({
      name: requiredText,
      nickname: Right.create,
      city: requiredText,
      state: requiredText
    });
    var handleAction = function(v) {
      if (v instanceof Initialize3) {
        return bind7(liftEffect3(bindFlipped7(getItem("local-storage-form"))(bindFlipped7(localStorage)(windowImpl))))(function(storedState) {
          var v1 = bindFlipped1(decodeJson2)(bindFlipped1(parseJson)(note(new TypeMismatch2("No data"))(storedState)));
          if (v1 instanceof Left) {
            return log4(printJsonDecodeError(v1.value0));
          }
          ;
          if (v1 instanceof Right) {
            return bind7(gets4(function(v2) {
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
        var fieldsJson = stringify(encodeJson2(v.value0.fields));
        return discard4(liftEffect3(bindFlipped7(setItem("local-storage-form")(fieldsJson))(bindFlipped7(localStorage)(windowImpl))))(function() {
          return put6(v.value0);
        });
      }
      ;
      if (v instanceof Eval6) {
        return $$eval(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Example.LocalStorage (line 56, column 18 - line 72, column 20): " + [v.constructor.name]);
    };
    return formless(monadEffectAff)(mkFieldStates1(hmapRecord()(mapRecordWithIndexCons6(constMapping6)(mapRecordWithIndexCons15(constMapping6)(mapRecordWithIndexCons24(constMapping6)(mapRecordWithIndexCons33(constMapping6)(mapRecordWithIndexNil)()())()())()())()())))(mkFieldActions1(hmapWithIndexRecord()(mapRecordWithIndexCons6(mappingWithIndexMkFieldAc(cityIsSymbol)(refl)())(mapRecordWithIndexCons15(mappingWithIndexMkFieldAc(nameIsSymbol4)(refl)())(mapRecordWithIndexCons24(mappingWithIndexMkFieldAc(nicknameIsSymbol)(refl)())(mapRecordWithIndexCons33(mappingWithIndexMkFieldAc(stateIsSymbol)(refl)())(mapRecordWithIndexNil)()())()())()())()())))(mkFieldResults1(hfoldlRecordWithIndex6(foldlRecordCons6(foldingWithIndexMkFieldRe(cityIsSymbol)(refl)()())(foldlRecordCons15(foldingWithIndexMkFieldRe(nameIsSymbol4)(refl)()())(foldlRecordCons24(foldingWithIndexMkFieldRe(nicknameIsSymbol)(refl)()())(foldlRecordCons33(foldingWithIndexMkFieldRe(stateIsSymbol)(refl)()())(foldlRecordNil)))))))(mkFieldOutputs1(hfoldlRecordWithIndex6(foldlRecordCons6(foldingWithIndexMkFieldOu(cityIsSymbol)(refl)()())(foldlRecordCons15(foldingWithIndexMkFieldOu(nameIsSymbol4)(refl)()())(foldlRecordCons24(foldingWithIndexMkFieldOu(nicknameIsSymbol)(refl)()())(foldlRecordCons33(foldingWithIndexMkFieldOu(stateIsSymbol)(refl)()())(foldlRecordNil)))))))(mkConfig1(defaultsRecord()()))({
      liftAction: Eval6.create
    })(mempty(monoidRecord()(monoidRecordCons(cityIsSymbol)(monoidString)()(monoidRecordCons(nameIsSymbol4)(monoidString)()(monoidRecordCons(nicknameIsSymbol)(monoidString)()(monoidRecordCons(stateIsSymbol)(monoidString)()(monoidRecordNil)))))))(mkComponent({
      initialState: identity(categoryFn),
      render: render3,
      "eval": mkEval({
        handleAction,
        handleQuery: handleQuery2,
        receive: function($256) {
          return Just.create(Receive7.create($256));
        },
        initialize: new Just(Initialize3.value),
        finalize: defaultEval.finalize
      })
    }));
  }();

  // output/Halogen.Aff.Util/index.js
  var bind8 = /* @__PURE__ */ bind(bindAff);
  var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var bindFlipped8 = /* @__PURE__ */ bindFlipped(bindEffect);
  var composeKleisliFlipped4 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
  var pure8 = /* @__PURE__ */ pure(applicativeAff);
  var bindFlipped12 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var map22 = /* @__PURE__ */ map(functorEffect);
  var selectElement = function(query4) {
    return bind8(liftEffect4(bindFlipped8(composeKleisliFlipped4(function() {
      var $16 = querySelector(query4);
      return function($17) {
        return $16(toParentNode($17));
      };
    }())(document))(windowImpl)))(function(mel) {
      return pure8(bindFlipped12(fromElement)(mel));
    });
  };
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do2() {
      var rs = bindFlipped8(readyState)(bindFlipped8(document)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map22(toEventTarget)(windowImpl)();
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
  var slot_2 = /* @__PURE__ */ slot_()({
    reflectSymbol: function() {
      return "child";
    }
  })(ordUnit);
  var identity12 = /* @__PURE__ */ identity(categoryFn);
  var _child = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var render = function(innerComponent) {
    return function(state3) {
      return slot_2(_child)(unit)(innerComponent)(state3);
    };
  };
  var proxy = function(innerComponent) {
    return mkComponent({
      initialState: identity12,
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
    var traverse_7 = traverse_(dictApplicative)(foldableMaybe);
    return function(f) {
      return unDriverStateX(function(st) {
        return traverse_7(f)(st.rendering);
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
  var traverse_4 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var bindFlipped9 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var lookup7 = /* @__PURE__ */ lookup3(ordSubscriptionId);
  var bind13 = /* @__PURE__ */ bind(bindAff);
  var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var discard5 = /* @__PURE__ */ discard(discardUnit);
  var discard12 = /* @__PURE__ */ discard5(bindAff);
  var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
  var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
  var fork3 = /* @__PURE__ */ fork2(monadForkAff);
  var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(foldableList);
  var pure9 = /* @__PURE__ */ pure(applicativeAff);
  var map23 = /* @__PURE__ */ map(functorCoyoneda);
  var parallel2 = /* @__PURE__ */ parallel(parallelAff);
  var map110 = /* @__PURE__ */ map(functorAff);
  var sequential2 = /* @__PURE__ */ sequential(parallelAff);
  var map24 = /* @__PURE__ */ map(functorMaybe);
  var insert9 = /* @__PURE__ */ insert4(ordSubscriptionId);
  var retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
  var $$delete5 = /* @__PURE__ */ $$delete2(ordForkId);
  var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
  var insert12 = /* @__PURE__ */ insert4(ordForkId);
  var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
  var lookup12 = /* @__PURE__ */ lookup3(ordForkId);
  var lookup22 = /* @__PURE__ */ lookup3(ordString);
  var foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
  var alter2 = /* @__PURE__ */ alter(ordString);
  var unsubscribe3 = function(sid) {
    return function(ref2) {
      return function __do2() {
        var v = read(ref2)();
        var subs = read(v.subscriptions)();
        return traverse_4(unsubscribe)(bindFlipped9(lookup7(sid))(subs))();
      };
    };
  };
  var queueOrRun = function(ref2) {
    return function(au) {
      return bind13(liftEffect5(read(ref2)))(function(v) {
        if (v instanceof Nothing) {
          return au;
        }
        ;
        if (v instanceof Just) {
          return liftEffect5(write(new Just(new Cons(au, v.value0)))(ref2));
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
      });
    };
  };
  var handleLifecycle = function(lchs) {
    return function(f) {
      return discard12(liftEffect5(write({
        initializers: Nil.value,
        finalizers: Nil.value
      })(lchs)))(function() {
        return bind13(liftEffect5(f))(function(result) {
          return bind13(liftEffect5(read(lchs)))(function(v) {
            return discard12(traverse_22(fork3)(v.finalizers))(function() {
              return discard12(parSequence_2(v.initializers))(function() {
                return pure9(result);
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
      return bind13(liftEffect5(read(ref2)))(function(v) {
        return liftEffect5(modify$prime(function(i2) {
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
        return bind13(liftEffect5(read(ref2)))(function(v) {
          return evalM(render3)(ref2)(v["component"]["eval"](new Query(map23(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
        });
      };
    };
  };
  var evalM = function(render3) {
    return function(initRef) {
      return function(v) {
        var evalChildQuery = function(ref2) {
          return function(cqb) {
            return bind13(liftEffect5(read(ref2)))(function(v1) {
              return unChildQueryBox(function(v2) {
                var evalChild = function(v3) {
                  return parallel2(bind13(liftEffect5(read(v3)))(function(dsx) {
                    return unDriverStateX(function(ds) {
                      return evalQ(render3)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };
                return map110(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
              })(cqb);
            });
          };
        };
        var go2 = function(ref2) {
          return function(v1) {
            if (v1 instanceof State) {
              return bind13(liftEffect5(read(ref2)))(function(v2) {
                var v3 = v1.value0(v2.state);
                if (unsafeRefEq(v2.state)(v3.value1)) {
                  return pure9(v3.value0);
                }
                ;
                if (otherwise) {
                  return discard12(liftEffect5(write({
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
                    return discard12(handleLifecycle(v2.lifecycleHandlers)(render3(v2.lifecycleHandlers)(ref2)))(function() {
                      return pure9(v3.value0);
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
              });
            }
            ;
            if (v1 instanceof Subscribe) {
              return bind13(fresh(SubscriptionId)(ref2))(function(sid) {
                return bind13(liftEffect5(subscribe(v1.value0(sid))(function(act) {
                  return handleAff(evalF(render3)(ref2)(new Action(act)));
                })))(function(finalize) {
                  return bind13(liftEffect5(read(ref2)))(function(v2) {
                    return discard12(liftEffect5(modify_2(map24(insert9(sid)(finalize)))(v2.subscriptions)))(function() {
                      return pure9(v1.value1(sid));
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Unsubscribe) {
              return discard12(liftEffect5(unsubscribe3(v1.value0)(ref2)))(function() {
                return pure9(v1.value1);
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
              return bind13(liftEffect5(read(ref2)))(function(v2) {
                return bind13(liftEffect5(read(v2.handlerRef)))(function(handler3) {
                  return discard12(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                    return pure9(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Par) {
              return sequential2(retractFreeAp2(hoistFreeAp(function() {
                var $118 = evalM(render3)(ref2);
                return function($119) {
                  return parallel2($118($119));
                };
              }())(v1.value0)));
            }
            ;
            if (v1 instanceof Fork) {
              return bind13(fresh(ForkId)(ref2))(function(fid) {
                return bind13(liftEffect5(read(ref2)))(function(v2) {
                  return bind13(liftEffect5($$new(false)))(function(doneRef) {
                    return bind13(fork3($$finally(liftEffect5(function __do2() {
                      modify_2($$delete5(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render3)(ref2)(v1.value0))))(function(fiber) {
                      return discard12(liftEffect5(unlessM2(read(doneRef))(modify_2(insert12(fid)(fiber))(v2.forks))))(function() {
                        return pure9(v1.value1(fid));
                      });
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Join) {
              return bind13(liftEffect5(read(ref2)))(function(v2) {
                return bind13(liftEffect5(read(v2.forks)))(function(forkMap) {
                  return discard12(traverse_32(joinFiber)(lookup12(v1.value0)(forkMap)))(function() {
                    return pure9(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Kill) {
              return bind13(liftEffect5(read(ref2)))(function(v2) {
                return bind13(liftEffect5(read(v2.forks)))(function(forkMap) {
                  return discard12(traverse_32(killFiber(error("Cancelled")))(lookup12(v1.value0)(forkMap)))(function() {
                    return pure9(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof GetRef) {
              return bind13(liftEffect5(read(ref2)))(function(v2) {
                return pure9(v1.value1(lookup22(v1.value0)(v2.refs)));
              });
            }
            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 139, column 33): " + [v1.constructor.name]);
          };
        };
        return foldFree2(go2(initRef))(v);
      };
    };
  };
  var evalF = function(render3) {
    return function(ref2) {
      return function(v) {
        if (v instanceof RefUpdate) {
          return liftEffect5(flip(modify_2)(ref2)(mapDriverState(function(st) {
            return {
              component: st.component,
              state: st.state,
              refs: alter2($$const(v.value1))(v.value0)(st.refs),
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
          return bind13(liftEffect5(read(ref2)))(function(v1) {
            return evalM(render3)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
          });
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
      };
    };
  };

  // output/Halogen.Aff.Driver/index.js
  var bind9 = /* @__PURE__ */ bind(bindEffect);
  var discard6 = /* @__PURE__ */ discard(discardUnit);
  var for_3 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var traverse_5 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
  var fork4 = /* @__PURE__ */ fork2(monadForkAff);
  var bindFlipped10 = /* @__PURE__ */ bindFlipped(bindEffect);
  var traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
  var traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
  var discard23 = /* @__PURE__ */ discard6(bindAff);
  var parSequence_3 = /* @__PURE__ */ parSequence_(parallelAff)(foldableList);
  var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var pure10 = /* @__PURE__ */ pure(applicativeEffect);
  var map25 = /* @__PURE__ */ map(functorEffect);
  var pure13 = /* @__PURE__ */ pure(applicativeAff);
  var when3 = /* @__PURE__ */ when(applicativeEffect);
  var renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
  var $$void8 = /* @__PURE__ */ $$void(functorAff);
  var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
  var renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
  var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEffect);
  var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
  var bind14 = /* @__PURE__ */ bind(bindAff);
  var liftEffect1 = /* @__PURE__ */ liftEffect(monadEffectEffect);
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
      return for_3(queue)(function() {
        var $58 = traverse_5(fork4);
        return function($59) {
          return handleAff($58(reverse2($59)));
        };
      }())();
    };
  };
  var cleanupSubscriptionsAndForks = function(v) {
    return function __do2() {
      bindFlipped10(traverse_23(traverse_33(unsubscribe)))(read(v.subscriptions))();
      write(Nothing.value)(v.subscriptions)();
      bindFlipped10(traverse_33(function() {
        var $60 = killFiber(error("finalized"));
        return function($61) {
          return handleAff($60($61));
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
                  initializers: new Cons(discard23(parSequence_3(reverse2(handlers.initializers)))(function() {
                    return discard23(parentInitializer)(function() {
                      return liftEffect6(function __do2() {
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
                  bindFlipped10(unDriverStateX(function() {
                    var $62 = render3(lchs);
                    return function($63) {
                      return $62(function(v) {
                        return v.selfRef;
                      }($63));
                    };
                  }()))(read($$var2))();
                  bindFlipped10(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
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
                return unComponentSlot(function(slot5) {
                  return function __do2() {
                    var childrenIn = map25(slot5.pop)(read(childrenInRef))();
                    var $$var2 = function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do3() {
                            flip(write)(st.handlerRef)(function() {
                              var $64 = maybe(pure13(unit))(handler3);
                              return function($65) {
                                return $64(slot5.output($65));
                              };
                            }())();
                            return handleAff(evalM(render3)(st.selfRef)(st["component"]["eval"](new Receive(slot5.input, unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }
                      ;
                      if (childrenIn instanceof Nothing) {
                        return runComponent(lchs)(function() {
                          var $66 = maybe(pure13(unit))(handler3);
                          return function($67) {
                            return $66(slot5.output($67));
                          };
                        }())(slot5.input)(slot5.component)();
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                    }();
                    var isDuplicate = map25(function($68) {
                      return isJust(slot5.get($68));
                    })(read(childrenOutRef))();
                    when3(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    modify_2(slot5.set($$var2))(childrenOutRef)();
                    return bind9(read($$var2))(renderStateX2(function(v) {
                      if (v instanceof Nothing) {
                        return $$throw("Halogen internal error: child was not initialized in renderChild");
                      }
                      ;
                      if (v instanceof Just) {
                        return pure10(renderSpec2.renderChild(v.value0));
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
              var shouldProcessHandlers = map25(isNothing)(read(v.pendingHandlers))();
              when3(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty4)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var handler3 = function() {
                var $69 = queueOrRun(v.pendingHandlers);
                var $70 = evalF(render3)(v.selfRef);
                return function($71) {
                  return $69($$void8($70($71)));
                };
              }();
              var childHandler = function() {
                var $72 = queueOrRun(v.pendingQueries);
                return function($73) {
                  return $72(handler3(Action.create($73)));
                };
              }();
              var rendering = renderSpec2.render(function($74) {
                return handleAff(handler3($74));
              })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
              var children2 = read(v.childrenOut)();
              var childrenIn = read(v.childrenIn)();
              foreachSlot2(childrenIn)(function(v1) {
                return function __do3() {
                  var childDS = read(v1)();
                  renderStateX_2(renderSpec2.removeChild)(childDS)();
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
              return when3(shouldProcessHandlers)(flip(tailRecM3)(unit)(function(v1) {
                return function __do3() {
                  var handlers = read(v.pendingHandlers)();
                  write(new Just(Nil.value))(v.pendingHandlers)();
                  traverse_23(function() {
                    var $75 = traverse_5(fork4);
                    return function($76) {
                      return handleAff($75(reverse2($76)));
                    };
                  }())(handlers)();
                  var mmore = read(v.pendingHandlers)();
                  var $51 = maybe(false)($$null)(mmore);
                  if ($51) {
                    return voidLeft3(write(Nothing.value)(v.pendingHandlers))(new Done(unit))();
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
              return foreachSlot2(st.children)(function(v) {
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
              return bind14(liftEffect6(read(disposed)))(function(v) {
                if (v) {
                  return pure13(Nothing.value);
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
                    var v2 = liftEffect1(read(v1.selfRef))();
                    return for_3(v2.rendering)(renderSpec2.dispose)();
                  };
                })(dsx)();
              });
            };
          };
        };
        return bind14(liftEffect6(newLifecycleHandlers))(function(lchs) {
          return bind14(liftEffect6($$new(false)))(function(disposed) {
            return handleLifecycle(lchs)(function __do2() {
              var sio = create();
              var dsx = bindFlipped10(read)(runComponent(lchs)(function() {
                var $77 = notify(sio.listener);
                return function($78) {
                  return liftEffect6($77($78));
                };
              }())(i2)(component))();
              return unDriverStateX(function(st) {
                return pure10({
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
  var map26 = /* @__PURE__ */ map(functorEffect);
  var parentNode2 = /* @__PURE__ */ function() {
    var $6 = map26(toMaybe);
    return function($7) {
      return $6(_parentNode($7));
    };
  }();
  var nextSibling = /* @__PURE__ */ function() {
    var $15 = map26(toMaybe);
    return function($16) {
      return $15(_nextSibling($16));
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
  var $$void9 = /* @__PURE__ */ $$void(functorEffect);
  var pure11 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var unwrap5 = /* @__PURE__ */ unwrap();
  var when4 = /* @__PURE__ */ when(applicativeEffect);
  var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
  var identity13 = /* @__PURE__ */ identity(categoryFn);
  var bind15 = /* @__PURE__ */ bind(bindAff);
  var liftEffect7 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var map27 = /* @__PURE__ */ map(functorEffect);
  var bindFlipped11 = /* @__PURE__ */ bindFlipped(bindEffect);
  var substInParent = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof Just && v2 instanceof Just) {
          return $$void9(insertBefore(v)(v1.value0)(v2.value0));
        }
        ;
        if (v1 instanceof Nothing && v2 instanceof Just) {
          return $$void9(appendChild(v)(v2.value0));
        }
        ;
        return pure11(unit);
      };
    };
  };
  var removeChild3 = function(v) {
    return function __do2() {
      var npn = parentNode2(v.node)();
      return traverse_6(function(pn) {
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
          var buildThunk2 = buildThunk(unwrap5)(spec);
          var $lazy_patch = $runtime_lazy8("patch", "Halogen.VDom.Driver", function() {
            return function(st, slot5) {
              if (st instanceof Just) {
                if (slot5 instanceof ComponentSlot) {
                  halt(st.value0);
                  return $lazy_renderComponentSlot(100)(slot5.value0);
                }
                ;
                if (slot5 instanceof ThunkSlot) {
                  var step$prime = step2(st.value0, slot5.value0);
                  return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                }
                ;
                throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot5.constructor.name]);
              }
              ;
              return $lazy_render(104)(slot5);
            };
          });
          var $lazy_render = $runtime_lazy8("render", "Halogen.VDom.Driver", function() {
            return function(slot5) {
              if (slot5 instanceof ComponentSlot) {
                return $lazy_renderComponentSlot(86)(slot5.value0);
              }
              ;
              if (slot5 instanceof ThunkSlot) {
                var step4 = buildThunk2(slot5.value0);
                return mkStep(new Step(extract2(step4), new Just(step4), $lazy_patch(89), done));
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot5.constructor.name]);
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
                  $$void9(appendChild(node)(toNode2(container)))();
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
                  when4(not2(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent2))();
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
        renderChild: identity13,
        removeChild: removeChild3,
        dispose: removeChild3
      };
    };
  };
  var runUI2 = function(component) {
    return function(i2) {
      return function(element3) {
        return bind15(liftEffect7(map27(toDocument)(bindFlipped11(document)(windowImpl))))(function(document2) {
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
  var bind10 = /* @__PURE__ */ bind(bindEffect);
  var map28 = /* @__PURE__ */ map(functorEffect);
  var bindFlipped13 = /* @__PURE__ */ bindFlipped(bindEffect);
  var join3 = /* @__PURE__ */ join(bindEffect);
  var apply3 = /* @__PURE__ */ apply(applyEffect);
  var pure14 = /* @__PURE__ */ pure(applicativeEffect);
  var voidRight2 = /* @__PURE__ */ voidRight(functorEffect);
  var getHash = /* @__PURE__ */ bind10(/* @__PURE__ */ bind10(windowImpl)(location))(/* @__PURE__ */ function() {
    var $16 = map28(function() {
      var $18 = fromMaybe("");
      var $19 = stripPrefix("#");
      return function($20) {
        return $18($19($20));
      };
    }());
    return function($17) {
      return $16(hash($17));
    };
  }());
  var foldHashes = function(cb) {
    return function(init3) {
      return function __do2() {
        var ref2 = bindFlipped13($$new)(bindFlipped13(init3)(getHash))();
        var win = map28(toEventTarget)(windowImpl)();
        var listener = eventListener(function(v) {
          return bindFlipped13(flip(write)(ref2))(join3(apply3(map28(cb)(read(ref2)))(getHash)));
        })();
        addEventListener2(hashchange)(listener)(false)(win)();
        return removeEventListener2(hashchange)(listener)(false)(win);
      };
    };
  };
  var matchesWith = function(dictFoldable) {
    var indexl2 = indexl(dictFoldable);
    return function(parser) {
      return function(cb) {
        var go2 = function(a2) {
          var $21 = maybe(pure14(a2))(function(b2) {
            return voidRight2(new Just(b2))(cb(a2)(b2));
          });
          var $22 = indexl2(0);
          return function($23) {
            return $21($22(parser($23)));
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
  var discard7 = /* @__PURE__ */ discard(discardUnit)(bindHalogenM);
  var $$void10 = /* @__PURE__ */ $$void(functorHalogenM);
  var modify7 = /* @__PURE__ */ modify(monadStateHalogenM);
  var pure15 = /* @__PURE__ */ pure(applicativeHalogenM);
  var mapFlipped3 = /* @__PURE__ */ mapFlipped(functorArray);
  var sortWith2 = /* @__PURE__ */ sortWith(ordString);
  var fromJust5 = /* @__PURE__ */ fromJust();
  var foldMapDefaultL2 = /* @__PURE__ */ foldMapDefaultL(foldableArray)(/* @__PURE__ */ monoidObject(semigroupArray));
  var toUnfoldable6 = /* @__PURE__ */ toUnfoldable2(unfoldableArray);
  var slot3 = /* @__PURE__ */ slot()({
    reflectSymbol: function() {
      return "child";
    }
  })(ordString);
  var bind11 = /* @__PURE__ */ bind(bindAff);
  var void1 = /* @__PURE__ */ $$void(functorAff);
  var liftEffect8 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var for_4 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
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
    return discard7($$void10(modify7(function(state3) {
      var $37 = {};
      for (var $38 in state3) {
        if ({}.hasOwnProperty.call(state3, $38)) {
          $37[$38] = state3[$38];
        }
        ;
      }
      ;
      $37.route = v.value0;
      return $37;
    })))(function() {
      return pure15(new Just(v.value1));
    });
  };
  var class_2 = function($64) {
    return class_(ClassName($64));
  };
  var renderStoryNames = function(v) {
    return function(items2) {
      var linkActiveClass = "Storybook-link is-active";
      return ul([class_2("Storybook-nav-list")])(mapFlipped3(sortWith2(function(v1) {
        return v1.name;
      })(items2))(function(item2) {
        return li_([a([class_2(function() {
          var $44 = v.route === item2.path;
          if ($44) {
            return linkActiveClass;
          }
          ;
          return "Storybook-link";
        }()), href("#" + fromJust5($$encodeURIComponent(item2.path)))])([text(item2.name)])]);
      }));
    };
  };
  var renderSidebar = function(stories2) {
    return function(state3) {
      var nameObj = foldMapDefaultL2(function(v) {
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
      var sorted = sortWith2(fst)(toUnfoldable6(nameObj));
      return div2([class_2("Storybook-nav")])(mapFlipped3(sorted)(function(v) {
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
        return slot3(_child2)(state3.route)(v.value0)(unit)(absurd);
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
      return bind11(runUI2(app(config))(unit)(body2))(function(app$prime) {
        return void1(liftEffect8(hashes(function(v) {
          return function(next) {
            return for_4($$decodeURIComponent(next))(function(next$prime) {
              return launchAff(app$prime.query(mkTell(RouteChange.create(next$prime))));
            });
          };
        })));
      });
    };
  };

  // output/Example.Main/index.js
  var slot4 = /* @__PURE__ */ slot()({
    reflectSymbol: function() {
      return "inner";
    }
  })(ordUnit);
  var identity14 = /* @__PURE__ */ identity(categoryFn);
  var modify_4 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var showRecord2 = /* @__PURE__ */ showRecord()();
  var showRecordFieldsCons2 = /* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "name";
    }
  });
  var bind16 = /* @__PURE__ */ bind(bindAff);
  var for_5 = /* @__PURE__ */ for_(applicativeAff)(foldableMaybe);
  var mkExample = function(dictShow) {
    var show4 = show(dictShow);
    return function(title4) {
      return function(description) {
        return function(formComponent) {
          var render3 = function(state3) {
            return article_([h1_([text(title4)]), p_([text(description)]), slot4($$Proxy.value)(unit)(formComponent)(unit)(identity14), function() {
              if (state3.result instanceof Nothing) {
                return text("");
              }
              ;
              if (state3.result instanceof Just) {
                return code_([text(show4(state3.result.value0))]);
              }
              ;
              throw new Error("Failed pattern match at Example.Main (line 111, column 9 - line 113, column 60): " + [state3.result.constructor.name]);
            }()]);
          };
          var handleAction = function(result) {
            return modify_4(function(v) {
              var $88 = {};
              for (var $89 in v) {
                if ({}.hasOwnProperty.call(v, $89)) {
                  $88[$89] = v[$89];
                }
                ;
              }
              ;
              $88.result = new Just(result);
              return $88;
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
      var component = mkExample(showRecord2(showRecordFieldsCons({
        reflectSymbol: function() {
          return "city";
        }
      })(showRecordFieldsCons2(showRecordFieldsCons({
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
        initialState: identity14,
        render: function(v) {
          return render3;
        },
        "eval": mkEval(defaultEval)
      });
      return new Tuple("", proxy(component));
    }();
    var fileUpload2 = function() {
      var component = mkExample(showRecord2(showRecordFieldsCons2(showRecordFieldsCons({
        reflectSymbol: function() {
          return "photo";
        }
      })(showRecordFieldsNil)(showRecord2(showRecordFieldsCons2(showRecordFieldsCons({
        reflectSymbol: function() {
          return "size";
        }
      })(showRecordFieldsNil)(showNumber))(showString))))(showString)))("File Upload")("A form with a file upload button and several validation functions. Useful to see how to implement more complex form fields and validation.")(form5);
      return new Tuple("3. File Upload", proxy(component));
    }();
    var dependentFields = function() {
      var component = mkExample(showRecord2(showRecordFieldsCons({
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
      var component = mkExample(showRecord2(showRecordFieldsCons2(showRecordFieldsCons({
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
      var component = mkExample(showRecord2(showRecordFieldsCons({
        reflectSymbol: function() {
          return "message";
        }
      })(showRecordFieldsCons2(showRecordFieldsNil)(showString))(showString)))("Basic")("A simple form that implements all fields manually, without app-specific helpers. Useful to see exactly how Formless should be used when implementing your own helper functions for your application.")(form2);
      return new Tuple("1. Basic", proxy(component));
    }();
    return fromFoldable2(foldableArray)([home, basic, checkboxRadio, fileUpload2, dependentFields, localStorage2]);
  }();
  var main2 = /* @__PURE__ */ launchAff_(/* @__PURE__ */ bind16(awaitLoad)(function() {
    return bind16(selectElement(".app"))(function(mbApp) {
      return for_5(mbApp)(function(app2) {
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
