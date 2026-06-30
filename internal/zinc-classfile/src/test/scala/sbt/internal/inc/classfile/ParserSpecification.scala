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
package classfile

import sbt.internal.util.ConsoleLogger

class ParserSpecification extends UnitSpec {

  val sampleClasses = List[Class[?]](
    this.getClass,
    classOf[java.lang.Integer],
    classOf[java.util.AbstractMap.SimpleEntry[String, String]],
    classOf[String],
    classOf[Thread],
    classOf[org.scalacheck.Properties],
    // exercises meta-annotation parsing
    classOf[java.lang.annotation.Retention]
    // I thought it would be nice to throw in a nested annotation example here,
    // but I couldn't find one that we could use without having to add another
    // JAR to the test classpath. it's fine, we have nested annotation testing
    // over in AnalyzeSpecification
  )

  for (c <- sampleClasses)
    "classfile.Parser" should s"not crash when parsing $c" in {
      val logger = ConsoleLogger()
      // logger.setLevel(sbt.util.Level.Debug)
      val classfile = Parser(sbt.io.IO.classfileLocation(c), logger)
      assert(classfile ne null)
      assert(classfile.types.nonEmpty)
    }

  it should "parse InnerClasses attribute for AbstractMap.SimpleEntry" in {
    val logger = ConsoleLogger()
    val c = classOf[java.util.AbstractMap.SimpleEntry[String, String]]
    val cf = Parser(sbt.io.IO.classfileLocation(c), logger)
    val innerClasses = cf.innerClasses
    assert(innerClasses.nonEmpty)
    val self = innerClasses.find(_.innerClassName == "java.util.AbstractMap$SimpleEntry")
    assert(self.isDefined)
    assert(self.get.outerClassName == "java.util.AbstractMap")
  }

  it should "parse InnerClasses attribute for AbstractMap" in {
    val logger = ConsoleLogger()
    val c = classOf[java.util.AbstractMap[?, ?]]
    val cf = Parser(sbt.io.IO.classfileLocation(c), logger)
    val innerClasses = cf.innerClasses
    val entry = innerClasses.find(_.innerClassName == "java.util.AbstractMap$SimpleEntry")
    assert(entry.isDefined)
    assert(entry.get.outerClassName == "java.util.AbstractMap")
    assert(entry.get.isPublic)
  }

  it should "expose FieldOrMethodInfo predicates against a JDK class" in {
    val logger = ConsoleLogger()
    val cf = Parser(sbt.io.IO.classfileLocation(classOf[java.util.AbstractMap[?, ?]]), logger)
    val ctor = cf.methods.find(_.isConstructor).getOrElse(fail("no <init> in AbstractMap"))
    assert(!ctor.isStatic)
    assert(ctor.name.contains("<init>"))
    val clinitOpt = cf.methods.find(_.isStaticInit)
    clinitOpt.foreach { clinit =>
      assert(clinit.isStatic)
      assert(clinit.name.contains("<clinit>"))
    }
    // AbstractMap declares abstract `entrySet`
    val entrySet = cf.methods.find(_.name.contains("entrySet")).getOrElse(fail("no entrySet"))
    assert(entrySet.isAbstract)
    assert(entrySet.isPublic)
  }

  it should "detect varargs on a varargs method" in {
    val logger = ConsoleLogger()
    val cf = Parser(sbt.io.IO.classfileLocation(classOf[String]), logger)
    // String.format(Locale, String, Object...) is varargs
    val format =
      cf.methods.find(m => m.name.contains("format") && m.isVarArgs).getOrElse(
        fail("no varargs format(...) found on String")
      )
    assert(format.isVarArgs)
    assert(format.isStatic)
    assert(format.isPublic)
  }
}
