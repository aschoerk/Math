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


public protocol ArithmeticType : Hashable, Comparable, ExpressibleByIntegerLiteral {
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
    static func +(_: Self, _: Self) -> Self
    static func +=(_: inout Self, _: Self)
    static func -(_: Self, _: Self) -> Self
    static func -=(_: inout Self, _: Self)
    static func *(_: Self, _: Self) -> Self
    static func *=(_: inout Self, _: Self)
    static func /(_: Self, _: Self) -> Self
    static func /=(_: inout Self, _: Self)
    static func %(_: Self, _: Self) -> Self
    static func %=(_: inout Self, _: Self)
}

public protocol FloatingPointArithmeticType : ArithmeticType, FloatingPoint, SignedNumber, ExpressibleByFloatLiteral {}
extension Double: FloatingPointArithmeticType {}
extension Float: FloatingPointArithmeticType {}

// Swift didn't put these in BitwiseOperationsType
public protocol BitsOperationsType : ArithmeticType, BitwiseOperations {
    static func <<(_: Self, _: Self) -> Self
    static func <<=(_: inout Self, _: Self)
    static func >>(_: Self, _: Self) -> Self
    static func >>=(_: inout Self, _: Self)
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
    init(_: Self, _:(_:Element) -> Element)
    init(_: Self, _: Self, _:(_:Element, _:Element) -> Element)
    init(_: Element, _: Self, _:(_:Element, _:Element) -> Element)
    init(_: Self, _: Element, _:(_:Element, _:Element) -> Element)
    static prefix func ++(_: inout Self) -> Self
    static postfix func ++(_: inout Self) -> Self
    static prefix func --(_: inout Self) -> Self
    static postfix func --(_: inout Self) -> Self
    static func +(_: Self, _: Self) -> Self
    static func +=(_: inout Self, _: Self)
    static func +(_: Element, _: Self) -> Self
    static func +(_: Self, _: Element) -> Self
    static func +=(_: inout Self, _: Element)
    static func -(_: Self, _: Self) -> Self
    static func -=(_: inout Self, _: Self)
    static func -(_: Element, _: Self) -> Self
    static func -(_: Self, _: Element) -> Self
    static func -=(_: inout Self, _: Element)
    static func *(_: Element, _: Self) -> Self
    static func *(_: Self, _: Element) -> Self
    static func *=(_: inout Self, _: Element)
    static func /(_: Element, _: Self) -> Self
    static func /(_: Self, _: Element) -> Self
    static func /=(_: inout Self, _: Element)
    static func %(_: Self, _: Self) -> Self
    static func %=(_: inout Self, _: Self)
    static func %(_: Element, _: Self) -> Self
    static func %(_: Self, _: Element) -> Self
    static func %=(_: inout Self, _: Element)
}

public extension MatrixType {
    public func index(after: Int) -> Int {
        return after + 1
    }
}

// This protocol is only Vector2, Vector3, and Vector4
public protocol VectorType : MatrixType, ExpressibleByArrayLiteral {
    associatedtype FloatVector
    associatedtype DoubleVector
    associatedtype Int32Vector
    associatedtype UInt32Vector
    associatedtype BooleanVector

    // T.BooleanVector == BooleanVector : Must use this key with mixed types.
    subscript(_:Int) -> Element { get set }
    init<T:VectorType>(_: T, _:(_:T.Element) -> Element)
    where T.BooleanVector == BooleanVector
    init<T1:VectorType, T2:VectorType>
    (_:T1, _:T2, _:(_:T1.Element, _:T2.Element) -> Element)
    where T1.BooleanVector == BooleanVector, T2.BooleanVector == BooleanVector
    init<T1:VectorType, T2:VectorType>
    (_:T1, _:inout T2, _:  (_:T1.Element, _:inout T2.Element) -> Element)
    where T1.BooleanVector == BooleanVector, T2.BooleanVector == BooleanVector
    init<T1:VectorType, T2:VectorType, T3:VectorType>
    (_:T1, _:T2, _:T3, _:(_:T1.Element, _:T2.Element, _:T3.Element) -> Element)
    where T1.BooleanVector == BooleanVector, T2.BooleanVector == BooleanVector, T3.BooleanVector == BooleanVector
    init<T1:VectorType, T2:VectorType, T3:BooleanVectorType>
    (_:T1, _:T2, _:T3, _:(_:T1.Element, _:T2.Element, _:Bool) -> Element)
    where T1.BooleanVector == BooleanVector, T2.BooleanVector == BooleanVector, T3.BooleanVector == BooleanVector

    static func *(_: Self, _: Self) -> Self
    static func *=(_: inout Self, _: Self)
    static func /(_: Self, _: Self) -> Self
    static func /=(_: inout Self, _: Self)
}


// This protocol is only Vector2b, Vector3b, and Vector4b
public protocol BooleanVectorType : MutableCollection, Hashable, Equatable, CustomDebugStringConvertible {
    associatedtype BooleanVector
    associatedtype Index = Int

    subscript(_:Int) -> Bool { get set }
    init(_: Self, _:(_:Bool) -> Bool)
    init<T:VectorType>(_: T, _:(_:T.Element) -> Bool)
    where T.BooleanVector == BooleanVector
    init<T1:VectorType, T2:VectorType>
            (_:T1, _:T2, _:(_:T1.Element, _:T2.Element) -> Bool)
    where T1.BooleanVector == BooleanVector, T2.BooleanVector == BooleanVector
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

public func radians<T:FloatingPointArithmeticType>(degrees:T) -> T {
    return degrees * 0.017453292519943295
}

public func radians<genType:VectorType>(degrees:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(degrees, radians)
}

public func degrees<T:FloatingPointArithmeticType>(radians:T) -> T {
    return radians * 57.29577951308232
}

public func degrees<genType:VectorType>(radians:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(radians, degrees)
}

public func sin<genType:VectorType>(angle:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(angle, SGLMath.SGLsin)
}

public func cos<genType:VectorType>(angle:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(angle, SGLMath.SGLcos)
}

public func tan<genType:VectorType>(angle:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(angle, SGLMath.SGLtan)
}

public func asin<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLasin)
}

public func acos<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLacos)
}

public func atan<genType:VectorType>(_ y:genType.Element, _ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(y, x, SGLMath.SGLatan)
}

public func atan<genType:VectorType>(_ y:genType, _ x:genType.Element) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(y, x, SGLMath.SGLatan)
}

public func atan<genType:VectorType>(_ y:genType, _ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(y, x, SGLMath.SGLatan)
}

public func atan<genType:VectorType>(yoverx:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(yoverx, SGLMath.SGLatan)
}

public func sinh<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLsinh)
}

public func cosh<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLcosh)
}

public func tanh<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLtanh)
}

public func asinh<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLasinh)
}

public func acosh<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLacosh)
}

public func atanh<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLatanh)
}


// Section 8.2 Exponential Functions

public func pow<genType:VectorType>(_ x:genType, _ y:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, y, SGLMath.SGLpow)
}

public func exp<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLexp)
}

public func log<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLlog)
}

public func exp2<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLexp2)
}

public func log2<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLlog2)
}

public func sqrt<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLsqrt)
}

public func inversesqrt<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType{
    return genType(x) { 1 / SGLMath.SGLsqrt($0) }
}


// Section 8.3 Common Functions

public func abs<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:AbsoluteValuable {
    return genType(x, abs)
}

public func sign<Vector: VectorType>(_ x:Vector) -> Vector
  where Vector.Element: SignedNumber{
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

public func floor<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLfloor)
}

public func trunc<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLtrunc)
}

public func round<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
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

public func roundEven<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, roundEven)
}

public func ceil<genType:VectorType>(_ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, SGLMath.SGLceil)
}

public func fract<T:VectorType>(_ x:T) -> T
  where T.Element:FloatingPointArithmeticType {
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

public func mod<genType:VectorType>
(_ x:genType, _ y:genType.Element) -> genType {
    return x % y
}

public func mod<genType:VectorType>
(_ x:genType, _ y:genType) -> genType {
    return x % y
}

public func modf<genType:VectorType>(_ x:genType, _ i:inout genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x, &i, SGLMath.SGLmodf)
}

public func min<genType:VectorType>(_ x:genType, _ y:genType) -> genType {
    return genType(x, y, min)
}

public func min<genType:VectorType>(_ x:genType, _ y:genType.Element) -> genType {
    return genType(x, y, min)
}

public func max<genType:VectorType>(_ x:genType, _ y:genType) -> genType {
    return genType(x, y, max)
}

public func max<genType:VectorType>(_ x:genType, _ y:genType.Element) -> genType {
    return genType(x, y, max)
}

public func clamp<genType:VectorType>(_ x:genType, _ minVal:genType, _ maxVal:genType) -> genType {
    return genType(x, minVal, maxVal) {
        (_ x:genType.Element, minVal:genType.Element, maxVal:genType.Element) in
        min(max(x, minVal), maxVal)
    }
}

public func clamp<genType:VectorType>
    (_ x:genType, _ minVal:genType.Element, _ maxVal:genType.Element) -> genType {
    return genType(x) { min(max($0, minVal), maxVal) }
}

public func mix<genType:VectorType>(_ x:genType, _ y:genType, _ a:genType) -> genType {
    return genType(x, y, a) {
        (_ x:genType.Element, y:genType.Element, a:genType.Element) in
        let t = x * (1 - a)
        return t + y * a
    }
}

public func mix<genType:VectorType>(_ x:genType, _ y:genType, _ a:genType.Element) -> genType {
    let inv = 1 - a
    return genType(x, y) {$0 * inv + $1 * a}
}

public func mix<genType:VectorType, genBType:BooleanVectorType>(_ x:genType, _ y:genType, _ a:genBType) -> genType
  where genType.BooleanVector == genBType.BooleanVector {
    return genType(x, y, a) {$2 ? $1 : $0}
}

public func step<genType:VectorType>(edge:genType, _ x:genType) -> genType {
    return genType(edge, x) { $1 < $0 ? 0 : 1}
}

public func step<genType:VectorType>(edge:genType.Element, _ x:genType) -> genType {
    return genType(x) { $0 < edge ? 0 : 1}
}

public func smoothstep<genType:VectorType>(edge0:genType, _ edge1:genType, _ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(edge0, edge1, x) { (edge0, edge1, x) in
        var i = x-edge0
        i /= edge1-edge0
        let t = min(max( i, 0), 1)
        i = 3 - 2 * t
        return t * t * i
    }
}

public func smoothstep<genType:VectorType>(edge0:genType.Element, _ edge1:genType.Element, _ x:genType) -> genType
  where genType.Element:FloatingPointArithmeticType {
    return genType(x) { (x) in
        var i = x-edge0
        i /= edge1-edge0
        let t = min(max( i, 0), 1)
        i = 3 - 2 * t
        return t * t * i
    }
}

public func isnan<genType:VectorType>(_ x:genType) -> genType.BooleanVector
  where
    genType.Element:FloatingPoint,
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
{
    return genType.BooleanVector(x) {$0.isNaN}
}

public func isinf<genType:VectorType>(_ x:genType) -> genType.BooleanVector
  where
    genType.Element:FloatingPoint,
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
{
    return genType.BooleanVector(x) {$0.isInfinite}
}

public func floatBitsToInt<genType:VectorType>(value:genType) -> genType.Int32Vector
  where
    genType.Int32Vector:VectorType,
    genType.Element == Float,
    genType.BooleanVector == genType.Int32Vector.BooleanVector
{
    return genType.Int32Vector( value ) {
        unsafeBitCast($0 , to: genType.Int32Vector.Element.self)
    }
}

public func floatBitsToUint<genType:VectorType>(value:genType) -> genType.UInt32Vector
  where
    genType.UInt32Vector:VectorType,
    genType.Element == Float,
    genType.BooleanVector == genType.UInt32Vector.BooleanVector
{
    return genType.UInt32Vector( value ) {
        unsafeBitCast($0 , to: genType.UInt32Vector.Element.self)
    }
}

public func intBitsToFloat<genType:VectorType>(value:genType) -> genType.FloatVector
  where
    genType.FloatVector:VectorType,
    genType.Element == Int32,
    genType.BooleanVector == genType.FloatVector.BooleanVector
{
    return genType.FloatVector( value ) {
        unsafeBitCast($0 , to: genType.FloatVector.Element.self)
    }
}

public func uintBitsToFloat<genType:VectorType>(value:genType) -> genType.FloatVector
  where
    genType.FloatVector:VectorType,
    genType.Element == UInt32,
    genType.BooleanVector == genType.FloatVector.BooleanVector
{
    return genType.FloatVector( value ) {
        unsafeBitCast($0 , to: genType.FloatVector.Element.self)
    }
}

public func fma<genType:VectorType>(_ a:genType, _ b:genType, _ c:genType) -> genType
  where
    genType.Element:FloatingPointArithmeticType
{
    return genType(a, b, c, SGLMath.SGLfma)
}

public func frexp<genType:VectorType, genIType:VectorType>(_ x:genType, _ exp:inout genIType) -> genType
  where
    genType.Element:FloatingPointArithmeticType,
    genIType.Element == Int32,
    genType.BooleanVector == genIType.BooleanVector
{
    return genType(x, &exp, SGLMath.SGLfrexp)
}

public func ldexp<genType:VectorType, genIType:VectorType>(_ x:genType, _ exp:genIType) -> genType
  where
    genType.Element:FloatingPointArithmeticType,
    genIType.Element == Int32,
    genType.BooleanVector == genIType.BooleanVector
{
    return genType(x, exp, SGLMath.SGLldexp)
}


// Section 8.4 Floating-Point Pack and Unpack Functions

public func packUnorm2x16(_ v:vec2) -> UInt32 {
    let i = uvec2(round(clamp(v, 0, 1) * 0xffff))
    return (i.y << 16) &+ i.x
}

public func packSnorm2x16(_ v:vec2) -> UInt32 {
    let i = ivec2(round(clamp(v, -1, 1) * 0x7fff))
    return ((UInt32(bitPattern: i.y) & 0xFFFF) << 16) &+ (UInt32(bitPattern: i.x) & 0xFFFF)
}

public func packUnorm4x8(_ v:vec4) -> UInt32 {
    let i = uvec4(round(clamp(v, 0, 1) * 0xff))
    return (i.w << 24) &+ (i.z << 16) &+ (i.y << 8) &+ i.x
}

public func packSnorm4x8(_ v:vec4) -> UInt32 {
    let i = ivec4(round(clamp(v, -1, 1) * 0x7f))
    var r = (UInt32(bitPattern: i.w) & 0xFF) << 24
    r += (UInt32(bitPattern: i.z) & 0xFF) << 16
    r += (UInt32(bitPattern: i.y) & 0xFF) << 8
    r += UInt32(bitPattern: i.x)
    return r
}

public func unpackUnorm2x16(_ p:UInt32) -> vec2 {
    let r = vec2(
        Float(p & 0xffff),
        Float(p >> 16 & 0xffff)
    )
    return r / 0xffff
}

public func unpackSnorm2x16(_ p:UInt32) -> vec2 {
    let p0 = UInt16(p >> 0 & 0xffff)
    let p1 = UInt16(p >> 16 & 0xffff)
    let r = vec2(
        Float(Int16(bitPattern: p0)),
        Float(Int16(bitPattern: p1))
    )
    return clamp(r / 0x7fff, -1, 1)
}

public func unpackUnorm4x8(_ p:UInt32) -> vec4 {
    let r = vec4(
        Float(p & 0xff),
        Float(p >> 8 & 0xff),
        Float(p >> 16 & 0xff),
        Float(p >> 24 & 0xff)
    )
    return r / 0xff
}

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

public func packDouble2x32 (_ v:uvec2) -> Double {
    let i:UInt64 = (UInt64(v.y) << 32) + UInt64(v.x)
    return unsafeBitCast(i, to: Double.self)
}

public func unpackDouble2x32 (_ v:Double) -> uvec2 {
    let d = unsafeBitCast(v, to: UInt64.self)
    return uvec2(
        UInt32( d & 0xFFFFFFFF ),
        UInt32( (d >> 32) & 0xFFFFFFFF )
    )
}

public func packHalf2x16 (_ v:vec2) -> UInt32 {
    var ret:UInt32 = UInt32(SGLMath.halfFromFloat(v[0]))
    ret += UInt32(SGLMath.halfFromFloat(v[1])) << 16
    return ret
}

public func unpackHalf2x16 (_ v:UInt32) -> vec2 {
    return vec2 (
        SGLMath.floatFromHalf( UInt16(v & 0xFFFF) ),
        SGLMath.floatFromHalf( UInt16((v>>16) & 0xFFFF) )
    )
}


// Section 8.5 Geometric Functions

public func length<genType:VectorType>(_ x:genType) -> genType.Element
  where
    genType.Element:FloatingPointArithmeticType
{
    return SGLMath.SGLsqrt(dot(x, x))
}

public func distance<genType:VectorType>(p0:genType, _ p1:genType) -> genType.Element
  where
    genType.Element:FloatingPointArithmeticType
{
    return length(p0 - p1)
}

public func dot<genType:VectorType>(_ x:genType, _ y:genType) -> genType.Element
  where
    genType.Element:FloatingPointArithmeticType
{
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

public func normalize<genType:VectorType>(_ x:genType) -> genType
where genType.Element:FloatingPointArithmeticType {
    return x / length(x)
}

public func faceforward<genType:VectorType>(_ n:genType, _ i:genType, _ nRef:genType) -> genType
where genType.Element:FloatingPointArithmeticType
{
    return dot(nRef, i) < 0 ? n : -n
}

public func reflect<genType:VectorType>(_ i:genType, _ n:genType) -> genType
where genType.Element:FloatingPointArithmeticType
{
    let f = 2 * dot(n, i)
    return i - f * n
}

public func refract<genType:VectorType>(_ i:genType, _ n:genType, _ eta:genType.Element) -> genType
where genType.Element:FloatingPointArithmeticType
{
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

public func matrixCompMult<mat:MatrixType>(_ x:mat, _ y:mat) -> mat {
    return mat(x, y, *)
}

public func outerProduct<T:ArithmeticType>(_ c:Vector2<T>, _ r:Vector2<T>) -> Matrix2x2<T> {
    return Matrix2x2(
        c * r[0], c * r[1]
    )
}

public func outerProduct<T:ArithmeticType>(_ c:Vector3<T>, _ r:Vector2<T>) -> Matrix2x3<T> {
    return Matrix2x3(
        c * r[0], c * r[1]
    )
}

public func outerProduct<T:ArithmeticType>(_ c:Vector4<T>, _ r:Vector2<T>) -> Matrix2x4<T> {
    return Matrix2x4(
        c * r[0], c * r[1]
    )
}

public func outerProduct<T:ArithmeticType>(c:Vector2<T>, _ r:Vector3<T>) -> Matrix3x2<T> {
    return Matrix3x2(
        c * r[0], c * r[1], c * r[2]
    )
}

public func outerProduct<T:ArithmeticType>(c:Vector3<T>, _ r:Vector3<T>) -> Matrix3x3<T> {
    return Matrix3x3(
        c * r[0], c * r[1], c * r[2]
    )
}

public func outerProduct<T:ArithmeticType>(c:Vector4<T>, _ r:Vector3<T>) -> Matrix3x4<T> {
    return Matrix3x4(
        c * r[0], c * r[1], c * r[2]
    )
}

public func outerProduct<T:ArithmeticType>(c:Vector2<T>, _ r:Vector4<T>) -> Matrix4x2<T> {
    let c1 = c * r[0]
    let c2 = c * r[1]
    let c3 = c * r[2]
    let c4 = c * r[3]

    return Matrix4x2(
        c1, c2, c3, c4
    )
}

public func outerProduct<T:ArithmeticType>(c:Vector3<T>, _ r:Vector4<T>) -> Matrix4x3<T> {

    let c1 = c * r[0]
    let c2 = c * r[1]
    let c3 = c * r[2]
    let c4 = c * r[3]

    return Matrix4x3(
        c1, c2, c3, c4
    )
}

public func outerProduct<T:ArithmeticType>(c:Vector4<T>, _ r:Vector4<T>) -> Matrix4x4<T> {
    let c1 = c * r[0]
    let c2 = c * r[1]
    let c3 = c * r[2]
    let c4 = c * r[3]

    return Matrix4x4(
        c1, c2, c3, c4
    )
}

public func transpose<T:ArithmeticType>(m:Matrix2x2<T>) -> Matrix2x2<T> {
    return m.transpose
}

public func transpose<T:ArithmeticType>(m:Matrix2x3<T>) -> Matrix3x2<T> {
    return m.transpose
}

public func transpose<T:ArithmeticType>(m:Matrix2x4<T>) -> Matrix4x2<T> {
    return m.transpose
}

public func transpose<T:ArithmeticType>(m:Matrix3x2<T>) -> Matrix2x3<T> {
    return m.transpose
}

public func transpose<T:ArithmeticType>(m:Matrix3x3<T>) -> Matrix3x3<T> {
    return m.transpose
}

public func transpose<T:ArithmeticType>(m:Matrix3x4<T>) -> Matrix4x3<T> {
    return m.transpose
}

public func transpose<T:ArithmeticType>(m:Matrix4x2<T>) -> Matrix2x4<T> {
    return m.transpose
}

public func transpose<T:ArithmeticType>(_ m:Matrix4x3<T>) -> Matrix3x4<T> {
    return m.transpose
}

public func transpose<T:ArithmeticType>(_ m:Matrix4x4<T>) -> Matrix4x4<T> {
    return m.transpose
}

public func determinant<T:ArithmeticType>(_ m:Matrix2x2<T>) -> T {
    return m.determinant
}

public func determinant<T:ArithmeticType>(_ m:Matrix3x3<T>) -> T {
    return m.determinant
}

public func determinant<T:ArithmeticType>(_ m:Matrix4x4<T>) -> T {
    return m.determinant
}

public func inverse<T:ArithmeticType>(m:Matrix2x2<T>) -> Matrix2x2<T> {
    return m.inverse
}

public func inverse<T:ArithmeticType>(m:Matrix3x3<T>) -> Matrix3x3<T> {
    return m.inverse
}

public func inverse<T:ArithmeticType>(m:Matrix4x4<T>) -> Matrix4x4<T> {
    return m.inverse
}


// Section 8.7 Vector Relational Functions

public func lessThan<genType:VectorType>(_ x:genType, _ y:genType) -> genType.BooleanVector
  where
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
{
    return genType.BooleanVector(x, y, <)
}

public func lessThanEqual<genType:VectorType>(_ x:genType, _ y:genType) -> genType.BooleanVector
  where
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
{
    return genType.BooleanVector(x, y, <=)
}

public func greaterThan<genType:VectorType>(_ x:genType, _ y:genType) -> genType.BooleanVector
  where
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
{
    return genType.BooleanVector(x, y, >)
}

public func greaterThanEqual<genType:VectorType>(_ x:genType, _ y:genType) -> genType.BooleanVector
  where
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
{
    return genType.BooleanVector(x, y, >=)
}

public func equal<genType:VectorType>(_ x:genType, _ y:genType) -> genType.BooleanVector
  where
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
{
    return genType.BooleanVector(x, y, ==)
}

public func notEqual<genType:VectorType>(_ x:genType, _ y:genType) -> genType.BooleanVector
  where
    genType.BooleanVector:BooleanVectorType,
    genType.BooleanVector == genType.BooleanVector.BooleanVector
{
    return genType.BooleanVector(x, y, !=)
}

public func any<bvec:BooleanVectorType>(_ x:bvec) -> bvec.Iterator.Element
  where
    bvec.Iterator.Element == Bool
{
    return x.reduce(false) { $0 || $1 }
}

public func all<bvec:BooleanVectorType>(_ x:bvec) -> bvec.Iterator.Element
  where
    bvec.Iterator.Element == Bool
{
    return x.reduce(true) { $0 && $1 }
}

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

public func +<T:MatrixType>(s: T.Element, x: T) -> T {
    return T(s, x, +)
}

public func +<T:MatrixType>(x: T, s: T.Element) -> T {
    return T(x, s, +)
}

public func +=<T:MatrixType>(x: inout T, s: T.Element) {
    x = x + s
}

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

public func -<T:MatrixType>(s: T.Element, x: T) -> T {
    return T(s, x, -)
}

public func -<T:MatrixType>(x: T, s: T.Element) -> T {
    return T(x, s, -)
}

public func -=<T:MatrixType>(x: inout T, s: T.Element) {
    x = x - s
}

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

public func /<T:MatrixType>(s: T.Element, x: T) -> T {
    return T(s, x, /)
}

public func /<T:MatrixType>(x: T, s: T.Element) -> T {
    return T(x, s, /)
}

public func /=<T:MatrixType>(x: inout T, s: T.Element) {
    x = x / s
}

public func %<T:MatrixType>(x1: T, x2: T) -> T {
    return T(x1, x2, %)
}

public func %=<T:MatrixType>(x1: inout T, x2: T) {
    x1 = x1 % x2
}

public func %<T:MatrixType>(s: T.Element, x: T) -> T {
    return T(s, x, %)
}

public func %<T:MatrixType>(x: T, s: T.Element) -> T {
    return T(x, s, %)
}

public func %=<T:MatrixType>(x: inout T, s: T.Element) {
    x = x % s
}


// Unchecked Integer Operators

public func &+<T:MatrixType>(v1: T, v2: T) -> T
where T.Element:IntegerArithmetic {
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

public func &+<T:MatrixType>(s: T.Element, v: T) -> T
where T.Element:IntegerArithmetic {
    return T(s, v, &+)
}

public func &+<T:MatrixType>(v: T, s: T.Element) -> T
where T.Element:IntegerArithmetic {
    return T(v, s, &+)
}

public func &-<T:MatrixType>(v1: T, v2: T) -> T
where T.Element:IntegerArithmetic {
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

public func &-<T:MatrixType>(s: T.Element, v: T) -> T
where T.Element:IntegerArithmetic {
    return T(s, v, &-)
}

public func &-<T:MatrixType>(v: T, s: T.Element) -> T
where T.Element:IntegerArithmetic {
    return T(v, s, &-)
}

public func &*<T:MatrixType>(v1: T, v2: T) -> T
where T.Element:IntegerArithmetic {
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

public func &*<T:MatrixType>(s: T.Element, v: T) -> T
where T.Element:IntegerArithmetic {
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

public func &*<T:MatrixType>(v: T, s: T.Element) -> T
where T.Element:IntegerArithmetic {
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

public func << <T:MatrixType>(v: T, s: T.Element) -> T
where T.Element:BitsOperationsType {
    return T(v, s, <<)
}

public func <<= <T:MatrixType>(v: inout T, s: T.Element)
where T.Element:BitsOperationsType {
    v = v << s
}

public func >> <T:MatrixType>(v: T, s: T.Element) -> T
where T.Element:BitsOperationsType {
    return T(v, s, <<)
}

public func >>= <T:MatrixType>(v: inout T, s: T.Element)
where T.Element:BitsOperationsType {
    v = v >> s
}

public func &<T:MatrixType>(x1: T, x2: T) -> T
where T.Element:BitsOperationsType {
    return T(x1, x2, &)
}

public func &=<T:MatrixType>(x1: inout T, x2: T)
where T.Element:BitsOperationsType {
    x1 = x1 & x2
}

public func &<T:MatrixType>(s: T.Element, x: T) -> T
where T.Element:BitsOperationsType {
    return T(s, x, &)
}

public func &<T:MatrixType>(x: T, s: T.Element) -> T
where T.Element:BitsOperationsType {
    return T(x, s, &)
}

public func &=<T:MatrixType>(x: inout T, s: T.Element)
where T.Element:BitsOperationsType {
    x = x & s
}

public func |<T:MatrixType>(x1: T, x2: T) -> T
where T.Element:BitsOperationsType {
    return T(x1, x2, |)
}

public func |=<T:MatrixType>(x1: inout T, x2: T)
where T.Element:BitsOperationsType {
    x1 = x1 | x2
}

public func |<T:MatrixType>(s: T.Element, x: T) -> T
where T.Element:BitsOperationsType {
    return T(s, x, |)
}

public func |<T:MatrixType>(x: T, s: T.Element) -> T
where T.Element:BitsOperationsType {
    return T(x, s, |)
}

public func |=<T:MatrixType>(x: inout T, s: T.Element)
where T.Element:BitsOperationsType {
    x = x | s
}

public func ^<T:MatrixType>(v1: T, v2: T) -> T
where T.Element:BitsOperationsType {
    return T(v1, v2, ^)
}

public func ^=<T:MatrixType>(x1: inout T, x2: T)
where T.Element:BitsOperationsType {
    x1 = x1 ^ x2
}

public func ^<T:MatrixType>(s: T.Element, x: T) -> T
where T.Element:BitsOperationsType {
    return T(s, x, ^)
}

public func ^<T:MatrixType>(x: T, s: T.Element) -> T
where T.Element:BitsOperationsType {
    return T(x, s, ^)
}

public func ^=<T:MatrixType>(x: inout T, s: T.Element)
where T.Element:BitsOperationsType{
    x = x ^ s
}

public prefix func ~<T:MatrixType>(v: T) -> T
where T.Element:BitsOperationsType {
    return T(v, ~)
}


// Signed Numbers Only

public prefix func +<T:MatrixType>(v: T) -> T
where T.Element:SignedNumber {
    return v
}

public prefix func -<T:MatrixType>(x: T) -> T
where T.Element:SignedNumber {
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
