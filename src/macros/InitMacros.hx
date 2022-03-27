package macros;

import haxe.macro.Expr;

class InitMacros 
{
    public static macro function InitVector(expr: Array<Expr>) {
        return macro {
            var vec = new haxe.ds.Vector($v{expr.length});
            $b{[for (i in 0...expr.length) macro vec.set($v{i}, ${expr[i]})]}
            vec;
        }
    }
}
