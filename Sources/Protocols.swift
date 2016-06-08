// Copyright (c) 2015-2016 David Turnbull
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and/or associated documentation files (the
// "Materials"), to deal in the Materials without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Materials, and to
// permit persons to whom the Materials are furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Materials.
//
// THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.


public protocol ArithmeticType : Hashable, Comparable, IntegerLiteralConvertible {
    init(_: Double)
    init(_: Float)
    init(_: Int)
    init(_: UInt)
    init(_: Int8)
    init(_: UInt8)
    init(_: Int16)
    init(_: UInt16)
    init(_: Int32)
    init(_: UInt32)
    init(_: Int64)
    init(_: UInt64)
    func +(_: Self, _: Self) -> Self
    func +=(_: inout Self, _: Self)
    func -(_: Self, _: Self) -> Self
    func -=(_: inout Self, _: Self)
    func *(_: Self, _: Self) -> Self
    func *=(_: inout Self, _: Self)
    func /(_: Self, _: Self) -> Self
    func /=(_: inout Self, _: Self)
    func %(_: Self, _: Self) -> Self
    func %=(_: inout Self, _: Self)
}

public protocol FloatingPointArithmeticType : ArithmeticType, FloatingPoint, SignedNumber, FloatLiteralConvertible {}
extension Double: FloatingPointArithmeticType {}
extension Float: FloatingPointArithmeticType {}

// Swift didn't put these in BitwiseOperationsType
public protocol BitsOperationsType : ArithmeticType, BitwiseOperations {
    func <<(_: Self, _: Self) -> Self
    func <<=(_: inout Self, _: Self)
    func >>(_: Self, _: Self) -> Self
    func >>=(_: inout Self, _: Self)
}
extension Int: BitsOperationsType {}
extension UInt: BitsOperationsType {}
extension Int8: BitsOperationsType {}
extension UInt8: BitsOperationsType {}
extension Int16: BitsOperationsType {}
extension UInt16: BitsOperationsType {}
extension Int32: BitsOperationsType {}
extension UInt32: BitsOperationsType {}
extension Int64: BitsOperationsType {}
extension UInt64: BitsOperationsType {}


// Anything not a plain single scalar is considered a Matrix.
// This includes Vectors, Complex, and Quaternion.
public protocol MatrixType : MutableCollection, Hashable, Equatable, CustomDebugStringConvertible {
    associatedtype Element:ArithmeticType
    associatedtype Index = Int
    
    init()
    init(_: Self, _:@noescape(_:Element) -> Element)
    init(_: Self, _: Self, _:@noescape(_:Element, _:Element) -> Element)
    init(_: Element, _: Self, _:@noescape(_:Element, _:Element) -> Element)
    init(_: Self, _: Element, _:@noescape(_:Element, _:Element) -> Element)
    prefix func ++(_: inout Self) -> Self
    postfix func ++(_: inout Self) -> Self
    prefix func --(_: inout Self) -> Self
    postfix func --(_: inout Self) -> Self
    func +(_: Self, _: Self) -> Self
    func +=(_: inout Self, _: Self)
    func +(_: Element, _: Self) -> Self
    func +(_: Self, _: Element) -> Self
    func +=(_: inout Self, _: Element)
    func -(_: Self, _: Self) -> Self
    func -=(_: inout Self, _: Self)
    func -(_: Element, _: Self) -> Self
    func -(_: Self, _: Element) -> Self
    func -=(_: inout Self, _: Element)
    func *(_: Element, _: Self) -> Self
    func *(_: Self, _: Element) -> Self
    func *=(_: inout Self, _: Element)
    func /(_: Element, _: Self) -> Self
    func /(_: Self, _: Element) -> Self
    func /=(_: inout Self, _: Element)
    func %(_: Self, _: Self) -> Self
    func %=(_: inout Self, _: Self)
    func %(_: Element, _: Self) -> Self
    func %(_: Self, _: Element) -> Self
    func %=(_: inout Self, _: Element)
}

public extension MatrixType {
    public func index(after: Int) -> Int {
        return after + 1
    }
}

// This protocol is only Vector2, Vector3, and Vector4
public protocol VectorType : MatrixType, ArrayLiteralConvertible {
    associatedtype FloatVector
    associatedtype DoubleVector
    associatedtype Int32Vector
    associatedtype UInt32Vector
    associatedtype BooleanVector
    
    // T.BooleanVector == BooleanVector : Must use this key with mixed types.
    subscript(_:Int) -> Element { get set }
    init<T:VectorType where T.BooleanVector == BooleanVector>(_: T, _:@noescape(_:T.Element) -> Element)
    init<T1:VectorType, T2:VectorType where
        T1.BooleanVector == BooleanVector, T2.BooleanVector == BooleanVector>
        (_:T1, _:T2, _:@noescape(_:T1.Element, _:T2.Element) -> Element)
    init<T1:VectorType, T2:VectorType where
        T1.BooleanVector == BooleanVector, T2.BooleanVector == BooleanVector>
        (_:T1, _:inout T2, _: @noescape (_:T1.Element, inout _:T2.Element) -> Element)
    init<T1:VectorType, T2:VectorType, T3:VectorType where
        T1.BooleanVector == BooleanVector, T2.BooleanVector == BooleanVector, T3.BooleanVector == BooleanVector>
        (_:T1, _:T2, _:T3, _:@noescape(_:T1.Element, _:T2.Element, _:T3.Element) -> Element)
    init<T1:VectorType, T2:VectorType, T3:BooleanVectorType where
        T1.BooleanVector == BooleanVector, T2.BooleanVector == BooleanVector, T3.BooleanVector == BooleanVector>
        (_:T1, _:T2, _:T3, _:@noescape(_:T1.Element, _:T2.Element, _:Bool) -> Element)
    func *(_: Self, _: Self) -> Self
    func *=(_: inout Self, _: Self)
    func /(_: Self, _: Self) -> Self
    func /=(_: inout Self, _: Self)
}


// This protocol is only Vector2b, Vector3b, and Vector4b
public protocol BooleanVectorType : MutableCollection, Hashable, Equatable, CustomDebugStringConvertible {
    associatedtype BooleanVector
    
    associatedtype Index = Int
    
    subscript(_:Int) -> Bool { get set }
    init(_: Self, _:@noescape(_:Bool) -> Bool)
    init<T:VectorType where T.BooleanVector == BooleanVector>(_: T, _:@noescape(_:T.Element) -> Bool)
    init<T1:VectorType, T2:VectorType where
        T1.BooleanVector == BooleanVector, T2.BooleanVector == BooleanVector>
        (_:T1, _:T2, _:@noescape(_:T1.Element, _:T2.Element) -> Bool)
}

public extension BooleanVectorType {
    public func index(after: Int) -> Int {
        return after + 1
    }
}

//MARK: glsl.swift

// Copyright (c) 2015-2016 David Turnbull
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and/or associated documentation files (the
// "Materials"), to deal in the Materials without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Materials, and to
// permit persons to whom the Materials are furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Materials.
//
// THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.


// Portability for the OpenGL Shading Language 4.50


// Sections 4.1.5 and 4.1.6

public typealias vec2 = Vector2<Float>
public typealias dvec2 = Vector2<Double>
public typealias ivec2 = Vector2<Int32>
public typealias uvec2 = Vector2<UInt32>
public typealias bvec2 = Vector2b

public typealias vec3 = Vector3<Float>
public typealias dvec3 = Vector3<Double>
public typealias ivec3 = Vector3<Int32>
public typealias uvec3 = Vector3<UInt32>
public typealias bvec3 = Vector3b

public typealias vec4 = Vector4<Float>
public typealias dvec4 = Vector4<Double>
public typealias ivec4 = Vector4<Int32>
public typealias uvec4 = Vector4<UInt32>
public typealias bvec4 = Vector4b

public typealias mat2 = Matrix2x2<Float>
public typealias dmat2 = Matrix2x2<Double>
public typealias mat3 = Matrix3x3<Float>
public typealias dmat3 = Matrix3x3<Double>
public typealias mat4 = Matrix4x4<Float>
public typealias dmat4 = Matrix4x4<Double>

public typealias mat2x2 = Matrix2x2<Float>
public typealias dmat2x2 = Matrix2x2<Double>
public typealias mat2x3 = Matrix2x3<Float>
public typealias dmat2x3 = Matrix2x3<Double>
public typealias mat2x4 = Matrix2x4<Float>
public typealias dmat2x4 = Matrix2x4<Double>

public typealias mat3x2 = Matrix3x2<Float>
public typealias dmat3x2 = Matrix3x2<Double>
public typealias mat3x3 = Matrix3x3<Float>
public typealias dmat3x3 = Matrix3x3<Double>
public typealias mat3x4 = Matrix3x4<Float>
public typealias dmat3x4 = Matrix3x4<Double>

public typealias mat4x2 = Matrix4x2<Float>
public typealias dmat4x2 = Matrix4x2<Double>
public typealias mat4x3 = Matrix4x3<Float>
public typealias dmat4x3 = Matrix4x3<Double>
public typealias mat4x4 = Matrix4x4<Float>
public typealias dmat4x4 = Matrix4x4<Double>


// Section 8.1 Angle and Trignometry Functions

@warn_unused_result
public func radians<T:FloatingPointArithmeticType>(degrees:T) -> T {
    return degrees * 0.017453292519943295
}

@warn_unused_result
public func radians<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(degrees:genType) -> genType {
    return genType(degrees, radians)
}

@warn_unused_result
public func degrees<T:FloatingPointArithmeticType>(radians:T) -> T {
    return radians * 57.29577951308232
}

@warn_unused_result
public func degrees<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(radians:genType) -> genType {
    return genType(radians, degrees)
}

@warn_unused_result
public func sin<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(angle:genType) -> genType {
    return genType(angle, SGLMath.SGLsin)
}

@warn_unused_result
public func cos<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(angle:genType) -> genType {
    return genType(angle, SGLMath.SGLcos)
}

@warn_unused_result
public func tan<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(angle:genType) -> genType {
    return genType(angle, SGLMath.SGLtan)
}

@warn_unused_result
public func asin<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLasin)
}

@warn_unused_result
public func acos<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLacos)
}

@warn_unused_result
public func atan<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ y:genType.Element, _ x:genType) -> genType {
    return genType(y, x, SGLMath.SGLatan)
}

@warn_unused_result
public func atan<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ y:genType, _ x:genType.Element) -> genType {
    return genType(y, x, SGLMath.SGLatan)
}

@warn_unused_result
public func atan<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ y:genType, _ x:genType) -> genType {
    return genType(y, x, SGLMath.SGLatan)
}

@warn_unused_result
public func atan<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(yoverx:genType) -> genType {
    return genType(yoverx, SGLMath.SGLatan)
}

@warn_unused_result
public func sinh<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLsinh)
}

@warn_unused_result
public func cosh<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLcosh)
}

@warn_unused_result
public func tanh<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLtanh)
}

@warn_unused_result
public func asinh<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLasinh)
}

@warn_unused_result
public func acosh<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLacosh)
}

@warn_unused_result
public func atanh<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLatanh)
}


// Section 8.2 Exponential Functions

@warn_unused_result
public func pow<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType, _ y:genType) -> genType {
    return genType(x, y, SGLMath.SGLpow)
}

@warn_unused_result
public func exp<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLexp)
}

@warn_unused_result
public func log<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLlog)
}

@warn_unused_result
public func exp2<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLexp2)
}

@warn_unused_result
public func log2<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLlog2)
}

@warn_unused_result
public func sqrt<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLsqrt)
}

@warn_unused_result
public func inversesqrt<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x) { 1 / SGLMath.SGLsqrt($0) }
}


// Section 8.3 Common Functions

@warn_unused_result
public func abs<genType:VectorType where
    genType.Element:AbsoluteValuable
    >(_ x:genType) -> genType {
    return genType(x, abs)
}

@warn_unused_result
public func sign<Vector: VectorType where
    Vector.Element: SignedNumber
    >(_ x:Vector) -> Vector {
    
    let function : (Vector.Element) -> Vector.Element = { component in
        if component == 0 {
            return 0
        } else if component < 0 {
            return -1
        } else {
            return 1
        }
    }
    
    return Vector(x, function)
}

@warn_unused_result
public func floor<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLfloor)
}

@warn_unused_result
public func trunc<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLtrunc)
}

@warn_unused_result
public func round<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLround)
}

private func roundEven<T:FloatingPointArithmeticType>(_ x:T) -> T {
    var int:T = 0
    let frac:T = SGLMath.SGLmodf(x, &int);
    if frac != T(0.5) && frac != T(-0.5) {
        return SGLMath.SGLround(x);
    }
    if int % 2 == 0 {
        return int
    }
    return int + (x <= 0 ? T(-1) : 1)
}

@warn_unused_result
public func roundEven<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, roundEven)
}

@warn_unused_result
public func ceil<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return genType(x, SGLMath.SGLceil)
}

@warn_unused_result
public func fract<T:VectorType where T.Element:FloatingPointArithmeticType>
    (_ x:T) -> T {
    let one_minus_ulp:T.Element
    switch(x) {
    case is Float:
        one_minus_ulp = T.Element(0x1.fffffep-1)
    case is Double:
        one_minus_ulp = T.Element(0x1.fffffffffffffp-1)
    default:
        preconditionFailure()
    }
    return min(x - floor(x), one_minus_ulp)
}

@warn_unused_result
public func mod<genType:VectorType>
    (_ x:genType, _ y:genType.Element) -> genType {
    return x % y
}

@warn_unused_result
public func mod<genType:VectorType>
    (_ x:genType, _ y:genType) -> genType {
    return x % y
}

@warn_unused_result
public func modf<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType, _ i:inout genType) -> genType {
    return genType(x, &i, SGLMath.SGLmodf)
}

@warn_unused_result
public func min<genType:VectorType>(_ x:genType, _ y:genType) -> genType {
    return genType(x, y, min)
}

@warn_unused_result
public func min<genType:VectorType>(_ x:genType, _ y:genType.Element) -> genType {
    return genType(x, y, min)
}

@warn_unused_result
public func max<genType:VectorType>(_ x:genType, _ y:genType) -> genType {
    return genType(x, y, max)
}

@warn_unused_result
public func max<genType:VectorType>(_ x:genType, _ y:genType.Element) -> genType {
    return genType(x, y, max)
}

@warn_unused_result
public func clamp<genType:VectorType>(_ x:genType, _ minVal:genType, _ maxVal:genType) -> genType {
    return genType(x, minVal, maxVal) {
        (_ x:genType.Element, minVal:genType.Element, maxVal:genType.Element) in
        min(max(x, minVal), maxVal)
    }
}

@warn_unused_result
public func clamp<genType:VectorType>
    (_ x:genType, _ minVal:genType.Element, _ maxVal:genType.Element) -> genType {
    return genType(x) { min(max($0, minVal), maxVal) }
}

@warn_unused_result
public func mix<genType:VectorType>(_ x:genType, _ y:genType, _ a:genType) -> genType {
    return genType(x, y, a) {
        (_ x:genType.Element, y:genType.Element, a:genType.Element) in
        let t = x * (1 - a)
        return t + y * a
    }
}

@warn_unused_result
public func mix<genType:VectorType>(_ x:genType, _ y:genType, _ a:genType.Element) -> genType {
    let inv = 1 - a
    return genType(x, y) {$0 * inv + $1 * a}
}

@warn_unused_result
public func mix<genType:VectorType, genBType:BooleanVectorType
    where genType.BooleanVector == genBType.BooleanVector
    >(_ x:genType, _ y:genType, _ a:genBType) -> genType {
    return genType(x, y, a) {$2 ? $1 : $0}
}

@warn_unused_result
public func step<genType:VectorType>(edge:genType, _ x:genType) -> genType {
    return genType(edge, x) { $1 < $0 ? 0 : 1}
}

@warn_unused_result
public func step<genType:VectorType>(edge:genType.Element, _ x:genType) -> genType {
    return genType(x) { $0 < edge ? 0 : 1}
}

@warn_unused_result
public func smoothstep<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(edge0:genType, _ edge1:genType, _ x:genType) -> genType {
    return genType(edge0, edge1, x) { (edge0, edge1, x) in
        var i = x-edge0
        i /= edge1-edge0
        let t = min(max( i, 0), 1)
        i = 3 - 2 * t
        return t * t * i
    }
}

@warn_unused_result
public func smoothstep<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(edge0:genType.Element, _ edge1:genType.Element, _ x:genType) -> genType {
    return genType(x) { (x) in
        var i = x-edge0
        i /= edge1-edge0
        let t = min(max( i, 0), 1)
        i = 3 - 2 * t
        return t * t * i
    }
}

@warn_unused_result
public func isnan<genType:VectorType
    where
    genType.Element:FloatingPoint,
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
    >(_ x:genType) -> genType.BooleanVector {
    return genType.BooleanVector(x) {$0.isNaN}
}

@warn_unused_result
public func isinf<genType:VectorType
    where
    genType.Element:FloatingPoint,
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
    >(_ x:genType) -> genType.BooleanVector {
    return genType.BooleanVector(x) {$0.isInfinite}
}

@warn_unused_result
public func floatBitsToInt<genType:VectorType where
    genType.Int32Vector:VectorType,
    genType.Element == Float,
    genType.BooleanVector == genType.Int32Vector.BooleanVector
    >(value:genType) -> genType.Int32Vector {
    return genType.Int32Vector( value ) {
        unsafeBitCast($0 , to: genType.Int32Vector.Element.self)
    }
}

@warn_unused_result
public func floatBitsToUint<genType:VectorType where
    genType.UInt32Vector:VectorType,
    genType.Element == Float,
    genType.BooleanVector == genType.UInt32Vector.BooleanVector
    >(value:genType) -> genType.UInt32Vector {
    return genType.UInt32Vector( value ) {
        unsafeBitCast($0 , to: genType.UInt32Vector.Element.self)
    }
}

@warn_unused_result
public func intBitsToFloat<genType:VectorType where
    genType.FloatVector:VectorType,
    genType.Element == Int32,
    genType.BooleanVector == genType.FloatVector.BooleanVector
    >(value:genType) -> genType.FloatVector {
    return genType.FloatVector( value ) {
        unsafeBitCast($0 , to: genType.FloatVector.Element.self)
    }
}

@warn_unused_result
public func uintBitsToFloat<genType:VectorType where
    genType.FloatVector:VectorType,
    genType.Element == UInt32,
    genType.BooleanVector == genType.FloatVector.BooleanVector
    >(value:genType) -> genType.FloatVector {
    return genType.FloatVector( value ) {
        unsafeBitCast($0 , to: genType.FloatVector.Element.self)
    }
}

@warn_unused_result
public func fma<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ a:genType, _ b:genType, _ c:genType) -> genType {
    return genType(a, b, c, SGLMath.SGLfma)
}

@warn_unused_result
public func frexp<genType:VectorType, genIType:VectorType where
    genType.Element:FloatingPointArithmeticType,
    genIType.Element == Int32,
    genType.BooleanVector == genIType.BooleanVector
    >(_ x:genType, _ exp:inout genIType) -> genType {
    return genType(x, &exp, SGLMath.SGLfrexp)
}

@warn_unused_result
public func ldexp<genType:VectorType, genIType:VectorType where
    genType.Element:FloatingPointArithmeticType,
    genIType.Element == Int32,
    genType.BooleanVector == genIType.BooleanVector
    >(_ x:genType, _ exp:genIType) -> genType {
    return genType(x, exp, SGLMath.SGLldexp)
}


// Section 8.4 Floating-Point Pack and Unpack Functions

@warn_unused_result
public func packUnorm2x16(_ v:vec2) -> UInt32 {
    let i = uvec2(round(clamp(v, 0, 1) * 0xffff))
    return (i.y << 16) &+ i.x
}

@warn_unused_result
public func packSnorm2x16(_ v:vec2) -> UInt32 {
    let i = ivec2(round(clamp(v, -1, 1) * 0x7fff))
    return ((UInt32(bitPattern: i.y) & 0xFFFF) << 16) &+ (UInt32(bitPattern: i.x) & 0xFFFF)
}

@warn_unused_result
public func packUnorm4x8(_ v:vec4) -> UInt32 {
    let i = uvec4(round(clamp(v, 0, 1) * 0xff))
    return (i.w << 24) &+ (i.z << 16) &+ (i.y << 8) &+ i.x
}

@warn_unused_result
public func packSnorm4x8(_ v:vec4) -> UInt32 {
    let i = ivec4(round(clamp(v, -1, 1) * 0x7f))
    var r = (UInt32(bitPattern: i.w) & 0xFF) << 24
    r += (UInt32(bitPattern: i.z) & 0xFF) << 16
    r += (UInt32(bitPattern: i.y) & 0xFF) << 8
    r += UInt32(bitPattern: i.x)
    return r
}

@warn_unused_result
public func unpackUnorm2x16(_ p:UInt32) -> vec2 {
    let r = vec2(
        Float(p & 0xffff),
        Float(p >> 16 & 0xffff)
    )
    return r / 0xffff
}

@warn_unused_result
public func unpackSnorm2x16(_ p:UInt32) -> vec2 {
    let p0 = UInt16(p >> 0 & 0xffff)
    let p1 = UInt16(p >> 16 & 0xffff)
    let r = vec2(
        Float(Int16(bitPattern: p0)),
        Float(Int16(bitPattern: p1))
    )
    return clamp(r / 0x7fff, -1, 1)
}

@warn_unused_result
public func unpackUnorm4x8(_ p:UInt32) -> vec4 {
    let r = vec4(
        Float(p & 0xff),
        Float(p >> 8 & 0xff),
        Float(p >> 16 & 0xff),
        Float(p >> 24 & 0xff)
    )
    return r / 0xff
}

@warn_unused_result
public func unpackSnorm4x8(_ p:UInt32) -> vec4 {
    let p0 = UInt8(p >> 0 & 0xff)
    let p1 = UInt8(p >> 8 & 0xff)
    let p2 = UInt8(p >> 16 & 0xff)
    let p3 = UInt8(p >> 24 & 0xff)
    let r = vec4(
        Float(Int8(bitPattern: p0)),
        Float(Int8(bitPattern: p1)),
        Float(Int8(bitPattern: p2)),
        Float(Int8(bitPattern: p3))
    )
    return clamp(r / 0x7f, -1, 1)
}

@warn_unused_result
public func packDouble2x32 (_ v:uvec2) -> Double {
    let i:UInt64 = (UInt64(v.y) << 32) + UInt64(v.x)
    return unsafeBitCast(i, to: Double.self)
}

@warn_unused_result
public func unpackDouble2x32 (_ v:Double) -> uvec2 {
    let d = unsafeBitCast(v, to: UInt64.self)
    return uvec2(
        UInt32( d & 0xFFFFFFFF ),
        UInt32( (d >> 32) & 0xFFFFFFFF )
    )
}

@warn_unused_result
public func packHalf2x16 (_ v:vec2) -> UInt32 {
    var ret:UInt32 = UInt32(SGLMath.halfFromFloat(v[0]))
    ret += UInt32(SGLMath.halfFromFloat(v[1])) << 16
    return ret
}

@warn_unused_result
public func unpackHalf2x16 (_ v:UInt32) -> vec2 {
    return vec2 (
        SGLMath.floatFromHalf( UInt16(v & 0xFFFF) ),
        SGLMath.floatFromHalf( UInt16((v>>16) & 0xFFFF) )
    )
}


// Section 8.5 Geometric Functions

@warn_unused_result
public func length<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType.Element {
    return SGLMath.SGLsqrt(dot(x, x))
}

@warn_unused_result
public func distance<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(p0:genType, _ p1:genType) -> genType.Element {
    return length(p0 - p1)
}

@warn_unused_result
public func dot<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType, _ y:genType) -> genType.Element {
    switch (x) {
    case is Vector2<genType.Element>:
        let xx = x as! Vector2<genType.Element>
        let yy = y as! Vector2<genType.Element>
        return xx.x * yy.x + xx.y * yy.y
    case is Vector3<genType.Element>:
        let xx = x as! Vector3<genType.Element>
        let yy = y as! Vector3<genType.Element>
        let z = xx.x * yy.x + xx.y * yy.y
        return z + xx.z * yy.z
    case is Vector4<genType.Element>:
        let xx = x as! Vector4<genType.Element>
        let yy = y as! Vector4<genType.Element>
        let z = xx.x * yy.x + xx.y * yy.y
        return z + xx.z * yy.z + xx.w * yy.w
    default:
        preconditionFailure()
    }
    // Above is a bit faster in debug builds
    //let a = genType(x, y, *)
    //return a.reduce(0) { $0 + ($1 as! genType.Element) }
}

@warn_unused_result
public func cross<T:FloatingPointArithmeticType>
    (_ x:Vector3<T>, _ y:Vector3<T>) -> Vector3<T> {
    var x1:T = x.y * y.z
    x1 = x1 - y.y * x.z
    var y1:T = x.z * y.x
    y1 = y1 - y.z * x.x
    var z1:T = x.x * y.y
    z1 = z1 - y.x * x.y
    return Vector3<T>(x1,y1,z1)
}

@warn_unused_result
public func normalize<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ x:genType) -> genType {
    return x / length(x)
}

@warn_unused_result
public func faceforward<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ n:genType, _ i:genType, _ nRef:genType) -> genType {
    return dot(nRef, i) < 0 ? n : -n
}

@warn_unused_result
public func reflect<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ i:genType, _ n:genType) -> genType {
    let f = 2 * dot(n, i)
    return i - f * n
}

@warn_unused_result
public func refract<genType:VectorType where
    genType.Element:FloatingPointArithmeticType
    >(_ i:genType, _ n:genType, _ eta:genType.Element) -> genType {
    let dotni = dot(n, i)
    var k = dotni * dotni
    k = 1 - k
    k = eta * eta * k
    k = 1 - k
    if (k < 0) { return genType() }
    let x = eta * dotni + SGLMath.SGLsqrt(k)
    let r = x * n
    return eta * i - r
}


// Section 8.6 Matrix Functions

@warn_unused_result
public func matrixCompMult<mat:MatrixType>(_ x:mat, _ y:mat) -> mat {
    return mat(x, y, *)
}

@warn_unused_result
public func outerProduct<T:ArithmeticType>(_ c:Vector2<T>, _ r:Vector2<T>) -> Matrix2x2<T> {
    return Matrix2x2(
        c * r[0], c * r[1]
    )
}

@warn_unused_result
public func outerProduct<T:ArithmeticType>(_ c:Vector3<T>, _ r:Vector2<T>) -> Matrix2x3<T> {
    return Matrix2x3(
        c * r[0], c * r[1]
    )
}

@warn_unused_result
public func outerProduct<T:ArithmeticType>(_ c:Vector4<T>, _ r:Vector2<T>) -> Matrix2x4<T> {
    return Matrix2x4(
        c * r[0], c * r[1]
    )
}

@warn_unused_result
public func outerProduct<T:ArithmeticType>(c:Vector2<T>, _ r:Vector3<T>) -> Matrix3x2<T> {
    return Matrix3x2(
        c * r[0], c * r[1], c * r[2]
    )
}

@warn_unused_result
public func outerProduct<T:ArithmeticType>(c:Vector3<T>, _ r:Vector3<T>) -> Matrix3x3<T> {
    return Matrix3x3(
        c * r[0], c * r[1], c * r[2]
    )
}

@warn_unused_result
public func outerProduct<T:ArithmeticType>(c:Vector4<T>, _ r:Vector3<T>) -> Matrix3x4<T> {
    return Matrix3x4(
        c * r[0], c * r[1], c * r[2]
    )
}

@warn_unused_result
public func outerProduct<T:ArithmeticType>(c:Vector2<T>, _ r:Vector4<T>) -> Matrix4x2<T> {
    let c1 = c * r[0]
    let c2 = c * r[1]
    let c3 = c * r[2]
    let c4 = c * r[3]
    
    return Matrix4x2(
        c1, c2, c3, c4
    )
}

@warn_unused_result
public func outerProduct<T:ArithmeticType>(c:Vector3<T>, _ r:Vector4<T>) -> Matrix4x3<T> {
    
    let c1 = c * r[0]
    let c2 = c * r[1]
    let c3 = c * r[2]
    let c4 = c * r[3]
    
    return Matrix4x3(
        c1, c2, c3, c4
    )
}

@warn_unused_result
public func outerProduct<T:ArithmeticType>(c:Vector4<T>, _ r:Vector4<T>) -> Matrix4x4<T> {
    let c1 = c * r[0]
    let c2 = c * r[1]
    let c3 = c * r[2]
    let c4 = c * r[3]
    
    return Matrix4x4(
        c1, c2, c3, c4
    )
}

@warn_unused_result
public func transpose<T:ArithmeticType>(m:Matrix2x2<T>) -> Matrix2x2<T> {
    return m.transpose
}

@warn_unused_result
public func transpose<T:ArithmeticType>(m:Matrix2x3<T>) -> Matrix3x2<T> {
    return m.transpose
}

@warn_unused_result
public func transpose<T:ArithmeticType>(m:Matrix2x4<T>) -> Matrix4x2<T> {
    return m.transpose
}

@warn_unused_result
public func transpose<T:ArithmeticType>(m:Matrix3x2<T>) -> Matrix2x3<T> {
    return m.transpose
}

@warn_unused_result
public func transpose<T:ArithmeticType>(m:Matrix3x3<T>) -> Matrix3x3<T> {
    return m.transpose
}

@warn_unused_result
public func transpose<T:ArithmeticType>(m:Matrix3x4<T>) -> Matrix4x3<T> {
    return m.transpose
}

@warn_unused_result
public func transpose<T:ArithmeticType>(m:Matrix4x2<T>) -> Matrix2x4<T> {
    return m.transpose
}

@warn_unused_result
public func transpose<T:ArithmeticType>(_ m:Matrix4x3<T>) -> Matrix3x4<T> {
    return m.transpose
}

@warn_unused_result
public func transpose<T:ArithmeticType>(_ m:Matrix4x4<T>) -> Matrix4x4<T> {
    return m.transpose
}

@warn_unused_result
public func determinant<T:ArithmeticType>(_ m:Matrix2x2<T>) -> T {
    return m.determinant
}

@warn_unused_result
public func determinant<T:ArithmeticType>(_ m:Matrix3x3<T>) -> T {
    return m.determinant
}

@warn_unused_result
public func determinant<T:ArithmeticType>(_ m:Matrix4x4<T>) -> T {
    return m.determinant
}

@warn_unused_result
public func inverse<T:ArithmeticType>(m:Matrix2x2<T>) -> Matrix2x2<T> {
    return m.inverse
}

@warn_unused_result
public func inverse<T:ArithmeticType>(m:Matrix3x3<T>) -> Matrix3x3<T> {
    return m.inverse
}

@warn_unused_result
public func inverse<T:ArithmeticType>(m:Matrix4x4<T>) -> Matrix4x4<T> {
    return m.inverse
}


// Section 8.7 Vector Relational Functions

@warn_unused_result
public func lessThan<genType:VectorType where
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
    >(_ x:genType, _ y:genType) -> genType.BooleanVector {
    return genType.BooleanVector(x, y, <)
}

@warn_unused_result
public func lessThanEqual<genType:VectorType where
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
    >(_ x:genType, _ y:genType) -> genType.BooleanVector {
    return genType.BooleanVector(x, y, <=)
}

@warn_unused_result
public func greaterThan<genType:VectorType where
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
    >(_ x:genType, _ y:genType) -> genType.BooleanVector {
    return genType.BooleanVector(x, y, >)
}

@warn_unused_result
public func greaterThanEqual<genType:VectorType where
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
    >(_ x:genType, _ y:genType) -> genType.BooleanVector {
    return genType.BooleanVector(x, y, >=)
}

@warn_unused_result
public func equal<genType:VectorType where
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
    >(_ x:genType, _ y:genType) -> genType.BooleanVector {
    return genType.BooleanVector(x, y, ==)
}

@warn_unused_result
public func notEqual<genType:VectorType where
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
    >(_ x:genType, _ y:genType) -> genType.BooleanVector {
    return genType.BooleanVector(x, y, !=)
}

@warn_unused_result
public func any<bvec:BooleanVectorType where
    bvec.Iterator.Element == Bool
    >(_ x:bvec) -> bvec.Iterator.Element {
    return x.reduce(false) { $0 || $1 }
}

@warn_unused_result
public func all<bvec:BooleanVectorType where
    bvec.Iterator.Element == Bool
    >(_ x:bvec) -> bvec.Iterator.Element {
    return x.reduce(true) { $0 && $1 }
}

@warn_unused_result
public func not<bvec:BooleanVectorType>(_ x:bvec) -> bvec {
    return bvec(x, !)
}


//MARK: Operators.swift

// Copyright (c) 2015-2016 David Turnbull
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and/or associated documentation files (the
// "Materials"), to deal in the Materials without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Materials, and to
// permit persons to whom the Materials are furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Materials.
//
// THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.


#if !os(Linux)
    import simd
#endif




// Arithmetic Operators

public prefix func ++<T:MatrixType>(_ v: inout T) -> T {
    v = v + 1
    return v
}

public postfix func ++<T:MatrixType>(_ v: inout T) -> T {
    let r = v
    v = v + 1
    return r
}

public prefix func --<T:MatrixType>(v: inout T) -> T {
    v = v - 1
    return v
}

public postfix func --<T:MatrixType>(v: inout T) -> T {
    let r = v
    v = v - 1
    return r
}

@warn_unused_result
public func +<T:MatrixType>(x1: T, x2: T) -> T {
    #if !os(Linux)
        switch (x1) {
        case is Matrix2x2<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float2x2.self) + unsafeBitCast(x2, to: float2x2.self), to: T.self)
        case is Matrix2x2<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double2x2.self) + unsafeBitCast(x2, to: double2x2.self), to: T.self)
        case is Matrix2x4<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float2x4.self) + unsafeBitCast(x2, to: float2x4.self), to: T.self)
        case is Matrix2x4<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double2x4.self) + unsafeBitCast(x2, to: double2x4.self), to: T.self)
        case is Matrix3x2<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float3x2.self) + unsafeBitCast(x2, to: float3x2.self), to: T.self)
        case is Matrix3x2<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double3x2.self) + unsafeBitCast(x2, to: double3x2.self), to: T.self)
        case is Matrix3x4<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float3x4.self) + unsafeBitCast(x2, to: float3x4.self), to: T.self)
        case is Matrix3x4<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double3x4.self) + unsafeBitCast(x2, to: double3x4.self), to: T.self)
        case is Matrix4x2<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float4x2.self) + unsafeBitCast(x2, to: float4x2.self), to: T.self)
        case is Matrix4x2<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double4x2.self) + unsafeBitCast(x2, to: double4x2.self), to: T.self)
        case is Matrix4x4<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float4x4.self) + unsafeBitCast(x2, to: float4x4.self), to: T.self)
        case is Matrix4x4<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double4x4.self) + unsafeBitCast(x2, to: double4x4.self), to: T.self)
        case is Vector2<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float2.self) + unsafeBitCast(x2, to: float2.self), to: T.self)
        case is Vector2<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double2.self) + unsafeBitCast(x2, to: double2.self), to: T.self)
        case is Vector4<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float4.self) + unsafeBitCast(x2, to: float4.self), to: T.self)
        case is Vector4<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double4.self) + unsafeBitCast(x2, to: double4.self), to: T.self)
        default: break
        }
    #endif
    return T(x1, x2, +)
}

public func +=<T:MatrixType>(x1: inout T, x2: T) {
    x1 = x1 + x2
}

@warn_unused_result
public func +<T:MatrixType>(s: T.Element, x: T) -> T {
    return T(s, x, +)
}

@warn_unused_result
public func +<T:MatrixType>(x: T, s: T.Element) -> T {
    return T(x, s, +)
}

public func +=<T:MatrixType>(x: inout T, s: T.Element) {
    x = x + s
}

@warn_unused_result
public func -<T:MatrixType>(x1: T, x2: T) -> T {
    #if !os(Linux)
        switch (x1) {
        case is Matrix2x2<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float2x2.self) - unsafeBitCast(x2, to: float2x2.self), to: T.self)
        case is Matrix2x2<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double2x2.self) - unsafeBitCast(x2, to: double2x2.self), to: T.self)
        case is Matrix2x4<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float2x4.self) - unsafeBitCast(x2, to: float2x4.self), to: T.self)
        case is Matrix2x4<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double2x4.self) - unsafeBitCast(x2, to: double2x4.self), to: T.self)
        case is Matrix3x2<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float3x2.self) - unsafeBitCast(x2, to: float3x2.self), to: T.self)
        case is Matrix3x2<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double3x2.self) - unsafeBitCast(x2, to: double3x2.self), to: T.self)
        case is Matrix3x4<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float3x4.self) - unsafeBitCast(x2, to: float3x4.self), to: T.self)
        case is Matrix3x4<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double3x4.self) - unsafeBitCast(x2, to: double3x4.self), to: T.self)
        case is Matrix4x2<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float4x2.self) - unsafeBitCast(x2, to: float4x2.self), to: T.self)
        case is Matrix4x2<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double4x2.self) - unsafeBitCast(x2, to: double4x2.self), to: T.self)
        case is Matrix4x4<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float4x4.self) - unsafeBitCast(x2, to: float4x4.self), to: T.self)
        case is Matrix4x4<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double4x4.self) - unsafeBitCast(x2, to: double4x4.self), to: T.self)
        case is Vector2<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float2.self) - unsafeBitCast(x2, to: float2.self), to: T.self)
        case is Vector2<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double2.self) - unsafeBitCast(x2, to: double2.self), to: T.self)
        case is Vector4<Float> :
            return unsafeBitCast(unsafeBitCast(x1, to: float4.self) - unsafeBitCast(x2, to: float4.self), to: T.self)
        case is Vector4<Double> :
            return unsafeBitCast(unsafeBitCast(x1, to: double4.self) - unsafeBitCast(x2, to: double4.self), to: T.self)
        default: break
        }
    #endif
    return T(x1, x2, -)
}

public func -=<T:MatrixType>(x1: inout T, x2: T) {
    x1 = x1 - x2
}

@warn_unused_result
public func -<T:MatrixType>(s: T.Element, x: T) -> T {
    return T(s, x, -)
}

@warn_unused_result
public func -<T:MatrixType>(x: T, s: T.Element) -> T {
    return T(x, s, -)
}

public func -=<T:MatrixType>(x: inout T, s: T.Element) {
    x = x - s
}

@warn_unused_result
public func *<T:MatrixType>(s: T.Element, x: T) -> T {
    #if !os(Linux)
        switch (x) {
        case is Matrix2x2<Float> :
            return unsafeBitCast((s as! Float) * unsafeBitCast(x, to: float2x2.self), to: T.self)
        case is Matrix2x2<Double> :
            return unsafeBitCast((s as! Double) * unsafeBitCast(x, to: double2x2.self), to: T.self)
        case is Matrix2x4<Float> :
            return unsafeBitCast((s as! Float) * unsafeBitCast(x, to: float2x4.self), to: T.self)
        case is Matrix2x4<Double> :
            return unsafeBitCast((s as! Double) * unsafeBitCast(x, to: double2x4.self), to: T.self)
        case is Matrix3x2<Float> :
            return unsafeBitCast((s as! Float) * unsafeBitCast(x, to: float3x2.self), to: T.self)
        case is Matrix3x2<Double> :
            return unsafeBitCast((s as! Double) * unsafeBitCast(x, to: double3x2.self), to: T.self)
        case is Matrix3x4<Float> :
            return unsafeBitCast((s as! Float) * unsafeBitCast(x, to: float3x4.self), to: T.self)
        case is Matrix3x4<Double> :
            return unsafeBitCast((s as! Double) * unsafeBitCast(x, to: double3x4.self), to: T.self)
        case is Matrix4x2<Float> :
            return unsafeBitCast((s as! Float) * unsafeBitCast(x, to: float4x2.self), to: T.self)
        case is Matrix4x2<Double> :
            return unsafeBitCast((s as! Double) * unsafeBitCast(x, to: double4x2.self), to: T.self)
        case is Matrix4x4<Float> :
            return unsafeBitCast((s as! Float) * unsafeBitCast(x, to: float4x4.self), to: T.self)
        case is Matrix4x4<Double> :
            return unsafeBitCast((s as! Double) * unsafeBitCast(x, to: double4x4.self), to: T.self)
        case is Vector2<Float> :
            return unsafeBitCast((s as! Float) * unsafeBitCast(x, to: float2.self), to: T.self)
        case is Vector2<Double> :
            return unsafeBitCast((s as! Double) * unsafeBitCast(x, to: double2.self), to: T.self)
        case is Vector4<Float> :
            return unsafeBitCast((s as! Float) * unsafeBitCast(x, to: float4.self), to: T.self)
        case is Vector4<Double> :
            return unsafeBitCast((s as! Double) * unsafeBitCast(x, to: double4.self), to: T.self)
        default: break
        }
    #endif
    return T(s, x, *)
}

@warn_unused_result
public func *<T:MatrixType>(x: T, s: T.Element) -> T {
    #if !os(Linux)
        switch (x) {
        case is Matrix2x2<Float> :
            return unsafeBitCast(unsafeBitCast(x, to: float2x2.self) * (s as! Float), to: T.self)
        case is Matrix2x2<Double> :
            return unsafeBitCast(unsafeBitCast(x, to: double2x2.self) * (s as! Double), to: T.self)
        case is Matrix2x4<Float> :
            return unsafeBitCast(unsafeBitCast(x, to: float2x4.self) * (s as! Float), to: T.self)
        case is Matrix2x4<Double> :
            return unsafeBitCast(unsafeBitCast(x, to: double2x4.self) * (s as! Double), to: T.self)
        case is Matrix3x2<Float> :
            return unsafeBitCast(unsafeBitCast(x, to: float3x2.self) * (s as! Float), to: T.self)
        case is Matrix3x2<Double> :
            return unsafeBitCast(unsafeBitCast(x, to: double3x2.self) * (s as! Double), to: T.self)
        case is Matrix3x4<Float> :
            return unsafeBitCast(unsafeBitCast(x, to: float3x4.self) * (s as! Float), to: T.self)
        case is Matrix3x4<Double> :
            return unsafeBitCast(unsafeBitCast(x, to: double3x4.self) * (s as! Double), to: T.self)
        case is Matrix4x2<Float> :
            return unsafeBitCast(unsafeBitCast(x, to: float4x2.self) * (s as! Float), to: T.self)
        case is Matrix4x2<Double> :
            return unsafeBitCast(unsafeBitCast(x, to: double4x2.self) * (s as! Double), to: T.self)
        case is Matrix4x4<Float> :
            return unsafeBitCast(unsafeBitCast(x, to: float4x4.self) * (s as! Float), to: T.self)
        case is Matrix4x4<Double> :
            return unsafeBitCast(unsafeBitCast(x, to: double4x4.self) * (s as! Double), to: T.self)
        case is Vector2<Float> :
            return unsafeBitCast(unsafeBitCast(x, to: float2.self) * (s as! Float), to: T.self)
        case is Vector2<Double> :
            return unsafeBitCast(unsafeBitCast(x, to: double2.self) * (s as! Double), to: T.self)
        case is Vector4<Float> :
            return unsafeBitCast(unsafeBitCast(x, to: float4.self) * (s as! Float), to: T.self)
        case is Vector4<Double> :
            return unsafeBitCast(unsafeBitCast(x, to: double4.self) * (s as! Double), to: T.self)
        default: break
        }
    #endif
    return T(x, s, *)
}

public func *=<T:MatrixType>(x: inout T, s: T.Element) {
    x = x * s
}

@warn_unused_result
public func /<T:MatrixType>(s: T.Element, x: T) -> T {
    return T(s, x, /)
}

@warn_unused_result
public func /<T:MatrixType>(x: T, s: T.Element) -> T {
    return T(x, s, /)
}

public func /=<T:MatrixType>(x: inout T, s: T.Element) {
    x = x / s
}

@warn_unused_result
public func %<T:MatrixType>(x1: T, x2: T) -> T {
    return T(x1, x2, %)
}

public func %=<T:MatrixType>(x1: inout T, x2: T) {
    x1 = x1 % x2
}

@warn_unused_result
public func %<T:MatrixType>(s: T.Element, x: T) -> T {
    return T(s, x, %)
}

@warn_unused_result
public func %<T:MatrixType>(x: T, s: T.Element) -> T {
    return T(x, s, %)
}

public func %=<T:MatrixType>(x: inout T, s: T.Element) {
    x = x % s
}


// Unchecked Integer Operators

@warn_unused_result
public func &+<T:MatrixType where T.Element:IntegerArithmetic>(v1: T, v2: T) -> T {
    #if !os(Linux)
        switch (v1) {
        case is Vector2<Int32>, is Vector2<UInt32> :
            return unsafeBitCast(unsafeBitCast(v1, to: int2.self) &+ unsafeBitCast(v2, to: int2.self), to: T.self)
        case is Vector4<Int32>, is Vector4<UInt32> :
            return unsafeBitCast(unsafeBitCast(v1, to: int4.self) &+ unsafeBitCast(v2, to: int4.self), to: T.self)
        default:
            break
        }
    #endif
    return T(v1, v2, &+)
}

@warn_unused_result
public func &+<T:MatrixType where T.Element:IntegerArithmetic>(s: T.Element, v: T) -> T {
    return T(s, v, &+)
}

@warn_unused_result
public func &+<T:MatrixType where T.Element:IntegerArithmetic>(v: T, s: T.Element) -> T {
    return T(v, s, &+)
}

@warn_unused_result
public func &-<T:MatrixType where T.Element:IntegerArithmetic>(v1: T, v2: T) -> T {
    #if !os(Linux)
        switch (v1) {
        case is Vector2<Int32>, is Vector2<UInt32> :
            return unsafeBitCast(unsafeBitCast(v1, to: int2.self) &- unsafeBitCast(v2, to: int2.self), to: T.self)
        case is Vector4<Int32>, is Vector4<UInt32> :
            return unsafeBitCast(unsafeBitCast(v1, to: int4.self) &- unsafeBitCast(v2, to: int4.self), to: T.self)
        default:
            break
        }
    #endif
    return T(v1, v2, &-)
}

@warn_unused_result
public func &-<T:MatrixType where T.Element:IntegerArithmetic>(s: T.Element, v: T) -> T {
    return T(s, v, &-)
}

@warn_unused_result
public func &-<T:MatrixType where T.Element:IntegerArithmetic>(v: T, s: T.Element) -> T {
    return T(v, s, &-)
}

@warn_unused_result
public func &*<T:MatrixType where T.Element:IntegerArithmetic>(v1: T, v2: T) -> T {
    #if !os(Linux)
        switch (v1) {
        case is Vector2<Int32>, is Vector2<UInt32> :
            return unsafeBitCast(unsafeBitCast(v1, to: int2.self) &* unsafeBitCast(v2, to: int2.self), to: T.self)
        case is Vector4<Int32>, is Vector4<UInt32> :
            return unsafeBitCast(unsafeBitCast(v1, to: int4.self) &* unsafeBitCast(v2, to: int4.self), to: T.self)
        default:
            break
        }
    #endif
    return T(v1, v2, &*)
}

@warn_unused_result
public func &*<T:MatrixType where T.Element:IntegerArithmetic>(s: T.Element, v: T) -> T {
    #if !os(Linux)
        switch (v) {
        case is Vector2<Int32>, is Vector2<UInt32> :
            return unsafeBitCast(unsafeBitCast(s, to: Int32.self) &* unsafeBitCast(v, to: int2.self), to: T.self)
        case is Vector4<Int32>, is Vector4<UInt32> :
            return unsafeBitCast(unsafeBitCast(s, to: Int32.self) &* unsafeBitCast(v, to: int4.self), to: T.self)
        default:
            break
        }
    #endif
    return T(s, v, &*)
}

@warn_unused_result
public func &*<T:MatrixType where T.Element:IntegerArithmetic>(v: T, s: T.Element) -> T {
    #if !os(Linux)
        switch (v) {
        case is Vector2<Int32>, is Vector2<UInt32> :
            return unsafeBitCast(unsafeBitCast(v, to: int2.self) &* unsafeBitCast(s, to: Int32.self), to: T.self)
        case is Vector4<Int32>, is Vector4<UInt32> :
            return unsafeBitCast(unsafeBitCast(v, to: int4.self) &* unsafeBitCast(s, to: Int32.self), to: T.self)
        default:
            break
        }
    #endif
    return T(v, s, &*)
}

@warn_unused_result
public func << <T:MatrixType where T.Element:BitsOperationsType>(v: T, s: T.Element) -> T {
    return T(v, s, <<)
}

public func <<= <T:MatrixType where T.Element:BitsOperationsType>(v: inout T, s: T.Element) {
    v = v << s
}

@warn_unused_result
public func >> <T:MatrixType where T.Element:BitsOperationsType>(v: T, s: T.Element) -> T {
    return T(v, s, <<)
}

public func >>= <T:MatrixType where T.Element:BitsOperationsType>(v: inout T, s: T.Element) {
    v = v >> s
}

@warn_unused_result
public func &<T:MatrixType where T.Element:BitsOperationsType>(x1: T, x2: T) -> T {
    return T(x1, x2, &)
}

public func &=<T:MatrixType where T.Element:BitsOperationsType>(x1: inout T, x2: T) {
    x1 = x1 & x2
}

@warn_unused_result
public func &<T:MatrixType where T.Element:BitsOperationsType>(s: T.Element, x: T) -> T {
    return T(s, x, &)
}

@warn_unused_result
public func &<T:MatrixType where T.Element:BitsOperationsType>(x: T, s: T.Element) -> T {
    return T(x, s, &)
}

public func &=<T:MatrixType where T.Element:BitsOperationsType>(x: inout T, s: T.Element) {
    x = x & s
}

@warn_unused_result
public func |<T:MatrixType where T.Element:BitsOperationsType>(x1: T, x2: T) -> T {
    return T(x1, x2, |)
}

public func |=<T:MatrixType where T.Element:BitsOperationsType>(x1: inout T, x2: T) {
    x1 = x1 | x2
}

@warn_unused_result
public func |<T:MatrixType where T.Element:BitsOperationsType>(s: T.Element, x: T) -> T {
    return T(s, x, |)
}

@warn_unused_result
public func |<T:MatrixType where T.Element:BitsOperationsType>(x: T, s: T.Element) -> T {
    return T(x, s, |)
}

public func |=<T:MatrixType where T.Element:BitsOperationsType>(x: inout T, s: T.Element) {
    x = x | s
}

@warn_unused_result
public func ^<T:MatrixType where T.Element:BitsOperationsType>(v1: T, v2: T) -> T {
    return T(v1, v2, ^)
}

public func ^=<T:MatrixType where T.Element:BitsOperationsType>(x1: inout T, x2: T) {
    x1 = x1 ^ x2
}

@warn_unused_result
public func ^<T:MatrixType where T.Element:BitsOperationsType>(s: T.Element, x: T) -> T {
    return T(s, x, ^)
}

@warn_unused_result
public func ^<T:MatrixType where T.Element:BitsOperationsType>(x: T, s: T.Element) -> T {
    return T(x, s, ^)
}

public func ^=<T:MatrixType where T.Element:BitsOperationsType>(x: inout T, s: T.Element) {
    x = x ^ s
}

@warn_unused_result
public prefix func ~<T:MatrixType where T.Element:BitsOperationsType>(v: T) -> T {
    return T(v, ~)
}


// Signed Numbers Only

@warn_unused_result
public prefix func +<T:MatrixType where T.Element:SignedNumber>
    (v: T) -> T {
    return v
}

@warn_unused_result
public prefix func -<T:MatrixType where T.Element:SignedNumber>
    (x: T) -> T {
    #if !os(Linux)
        switch (x) {
        case is Matrix2x2<Float> :
            return unsafeBitCast(-unsafeBitCast(x, to: float2x2.self), to: T.self)
        case is Matrix2x2<Double> :
            return unsafeBitCast(-unsafeBitCast(x, to: double2x2.self), to: T.self)
        case is Matrix2x4<Float> :
            return unsafeBitCast(-unsafeBitCast(x, to: float2x4.self), to: T.self)
        case is Matrix2x4<Double> :
            return unsafeBitCast(-unsafeBitCast(x, to: double2x4.self), to: T.self)
        case is Matrix3x2<Float> :
            return unsafeBitCast(-unsafeBitCast(x, to: float3x2.self), to: T.self)
        case is Matrix3x2<Double> :
            return unsafeBitCast(-unsafeBitCast(x, to: double3x2.self), to: T.self)
        case is Matrix3x4<Float> :
            return unsafeBitCast(-unsafeBitCast(x, to: float3x4.self), to: T.self)
        case is Matrix3x4<Double> :
            return unsafeBitCast(-unsafeBitCast(x, to: double3x4.self), to: T.self)
        case is Matrix4x2<Float> :
            return unsafeBitCast(-unsafeBitCast(x, to: float4x2.self), to: T.self)
        case is Matrix4x2<Double> :
            return unsafeBitCast(-unsafeBitCast(x, to: double4x2.self), to: T.self)
        case is Matrix4x4<Float> :
            return unsafeBitCast(-unsafeBitCast(x, to: float4x4.self), to: T.self)
        case is Matrix4x4<Double> :
            return unsafeBitCast(-unsafeBitCast(x, to: double4x4.self), to: T.self)
        case is Vector2<Float> :
            return unsafeBitCast(-unsafeBitCast(x, to: float2.self), to: T.self)
        case is Vector2<Double> :
            return unsafeBitCast(-unsafeBitCast(x, to: double2.self), to: T.self)
        case is Vector2<Int32>:
            return unsafeBitCast(-unsafeBitCast(x, to: int2.self), to: T.self)
        case is Vector4<Float> :
            return unsafeBitCast(-unsafeBitCast(x, to: float4.self), to: T.self)
        case is Vector4<Double> :
            return unsafeBitCast(-unsafeBitCast(x, to: double4.self), to: T.self)
        case is Vector4<Int32>:
            return unsafeBitCast(-unsafeBitCast(x, to: int4.self), to: T.self)
        default: break
        }
    #endif
    return T(x, -)
}


// Vector Multiply and Divide

@warn_unused_result
public func *<T:VectorType>(v1: T, v2: T) -> T {
    #if !os(Linux)
        switch (v1) {
        case is Vector2<Float> :
            return unsafeBitCast(unsafeBitCast(v1, to: float2.self) * unsafeBitCast(v2, to: float2.self), to: T.self)
        case is Vector2<Double> :
            return unsafeBitCast(unsafeBitCast(v1, to: double2.self) * unsafeBitCast(v2, to: double2.self), to: T.self)
        case is Vector4<Float> :
            return unsafeBitCast(unsafeBitCast(v1, to: float4.self) * unsafeBitCast(v2, to: float4.self), to: T.self)
        case is Vector4<Double> :
            return unsafeBitCast(unsafeBitCast(v1, to: double4.self) * unsafeBitCast(v2, to: double4.self), to: T.self)
        default: break
        }
    #endif
    return T(v1, v2, *)
}

public func *=<T:VectorType>(v1: inout T, v2: T) {
    v1 = v1 * v2
}

@warn_unused_result
public func /<T:VectorType>(v1: T, v2: T) -> T {
    #if !os(Linux)
        switch (v1) {
        case is Vector2<Int32>, is Vector2<UInt32> :
            return unsafeBitCast(unsafeBitCast(v1, to: int2.self) / unsafeBitCast(v2, to: int2.self), to: T.self)
        case is Vector4<Int32>, is Vector4<UInt32> :
            return unsafeBitCast(unsafeBitCast(v1, to: int4.self) / unsafeBitCast(v2, to: int4.self), to: T.self)
        case is Vector2<Float> :
            return unsafeBitCast(unsafeBitCast(v1, to: float2.self) / unsafeBitCast(v2, to: float2.self), to: T.self)
        case is Vector2<Double> :
            return unsafeBitCast(unsafeBitCast(v1, to: double2.self) / unsafeBitCast(v2, to: double2.self), to: T.self)
        case is Vector4<Float> :
            return unsafeBitCast(unsafeBitCast(v1, to: float4.self) / unsafeBitCast(v2, to: float4.self), to: T.self)
        case is Vector4<Double> :
            return unsafeBitCast(unsafeBitCast(v1, to: double4.self) / unsafeBitCast(v2, to: double4.self), to: T.self)
        default:
            break
        }
    #endif
    return T(v1, v2, /)
}

public func /=<T:VectorType>(v1: inout T, v2: T) {
    v1 = v1 / v2
}