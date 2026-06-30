/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend, and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * SPDX-License-Identifier: Apache-2.0
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package sbt
package internal
package inc

import xsbti.api

class DescriptorParserSpecification extends UnitSpec {

  private def projectionId(t: api.Type): Option[String] = t match {
    case p: api.Projection => Some(p.id)
    case _                 => None
  }

  private def arrayComponent(t: api.Type): Option[api.Type] = t match {
    case p: api.Parameterized if projectionId(p.baseType).contains("Array") =>
      p.typeArguments.headOption
    case _ => None
  }

  "DescriptorParser.fieldType" should "parse primitive descriptors" in {
    assert(projectionId(DescriptorParser.fieldType("I")).contains("Int"))
    assert(projectionId(DescriptorParser.fieldType("J")).contains("Long"))
    assert(projectionId(DescriptorParser.fieldType("Z")).contains("Boolean"))
    assert(projectionId(DescriptorParser.fieldType("D")).contains("Double"))
    assert(projectionId(DescriptorParser.fieldType("F")).contains("Float"))
    assert(projectionId(DescriptorParser.fieldType("B")).contains("Byte"))
    assert(projectionId(DescriptorParser.fieldType("C")).contains("Char"))
    assert(projectionId(DescriptorParser.fieldType("S")).contains("Short"))
  }

  it should "parse object descriptors and translate slashes to dots" in {
    val t = DescriptorParser.fieldType("Ljava/lang/String;")
    assert(projectionId(t).contains("String"))
  }

  it should "parse one-dimensional array descriptors" in {
    val t = DescriptorParser.fieldType("[I")
    val comp = arrayComponent(t).getOrElse(fail(s"not an Array: $t"))
    assert(projectionId(comp).contains("Int"))
  }

  it should "parse nested array descriptors" in {
    val t = DescriptorParser.fieldType("[[Ljava/lang/String;")
    val outer = arrayComponent(t).getOrElse(fail(s"outer not Array: $t"))
    val inner = arrayComponent(outer).getOrElse(fail(s"inner not Array: $outer"))
    assert(projectionId(inner).contains("String"))
  }

  "DescriptorParser.methodTypes" should "parse void no-arg" in {
    val (params, ret) = DescriptorParser.methodTypes("()V")
    assert(params.length === 0)
    assert(ret.isInstanceOf[api.EmptyType])
  }

  it should "parse parameter types and a non-void return type" in {
    val (params, ret) = DescriptorParser.methodTypes("(ILjava/lang/String;)Ljava/lang/Object;")
    assert(params.length === 2)
    assert(projectionId(params(0)).contains("Int"))
    assert(projectionId(params(1)).contains("String"))
    assert(projectionId(ret).contains("Object"))
  }

  it should "parse main signature" in {
    val (params, ret) = DescriptorParser.methodTypes("([Ljava/lang/String;)V")
    assert(params.length === 1)
    val arg = arrayComponent(params(0)).getOrElse(fail(s"arg not Array: ${params(0)}"))
    assert(projectionId(arg).contains("String"))
    assert(ret.isInstanceOf[api.EmptyType])
  }

  it should "reject malformed descriptors" in {
    assertThrows[IllegalArgumentException](DescriptorParser.methodTypes("garbage"))
    assertThrows[IllegalArgumentException](DescriptorParser.fieldType("Ljava/lang/String"))
  }
}
