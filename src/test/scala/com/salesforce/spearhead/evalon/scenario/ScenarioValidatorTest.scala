/*
 * Copyright (c) 2025, Salesforce, Inc.
 * SPDX-License-Identifier: Apache-2
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.salesforce.spearhead.evalon.scenario

import java.nio.file.Files

import org.scalatest.funsuite.AnyFunSuite

import com.salesforce.spearhead.evalon.model.*

class ScenarioValidatorTest extends AnyFunSuite:

  private def scenarioWith(criteria: EvalCriterion*): Scenario =
    Scenario(
      name = "s",
      description = "d",
      participants = Map.empty,
      conversations = Nil,
      evalCriteria = criteria.toList,
    )

  private def rubric(
      name: String = "clarity",
      description: String = "Levels: generic (0.5), clear (0.75)",
      passThreshold: Option[Double] = Some(0.75),
  ): EvalCriterion =
    EvalCriterion(name, description, CriterionType.Rubric, requireToolCall = false, passThreshold)

  test("a rubric with a threshold and a description is valid") {
    assert(ScenarioValidator.validate(scenarioWith(rubric())).isEmpty)
  }

  test("rubric thresholds of exactly 0 and 1 are valid") {
    assert(ScenarioValidator.validate(scenarioWith(rubric(passThreshold = Some(0.0)))).isEmpty)
    assert(ScenarioValidator.validate(scenarioWith(rubric(passThreshold = Some(1.0)))).isEmpty)
  }

  test("a rubric without a valid threshold is rejected") {
    List(None, Some(1.5), Some(-0.1), Some(Double.NaN)).foreach { threshold =>
      val problems = ScenarioValidator.validate(scenarioWith(rubric(passThreshold = threshold)))
      val expected = "Criterion 'clarity' is a rubric and needs a passThreshold between 0 and 1"
      assert(problems == List(expected), threshold)
    }
  }

  test("a rubric with a blank description is rejected") {
    val problems = ScenarioValidator.validate(scenarioWith(rubric(description = "  ")))
    val expected = "Criterion 'clarity' is a rubric and needs a description with its levels"
    assert(problems == List(expected))
  }

  test("a rubric with a blank name is labelled by its position") {
    val valid = rubric(name = "first")
    val problems =
      ScenarioValidator.validate(scenarioWith(valid, rubric(name = " ", passThreshold = None)))
    assert(problems == List("Criterion '#2' is a rubric and needs a passThreshold between 0 and 1"))
  }

  test("binary and scored criteria are not validated") {
    val scenario = scenarioWith(
      EvalCriterion("b", "", CriterionType.Binary, requireToolCall = false, Some(5.0)),
      EvalCriterion("s", "", CriterionType.Scored, requireToolCall = false, None),
      EvalCriterion("s2", "d", CriterionType.Scored, requireToolCall = false, Some(1.5)),
    )
    assert(ScenarioValidator.validate(scenario).isEmpty)
  }

  test("ScenarioLoader rejects a YAML rubric without pass_threshold") {
    val yaml =
      """name: rubric-scenario
        |description: d
        |eval_criteria:
        |  - name: clarity
        |    description: "Levels: generic (0.5), clear (0.75)"
        |    criterion_type: rubric
        |""".stripMargin
    val path = Files.createTempFile("rubric-scenario", ".yaml")
    try
      Files.writeString(path, yaml)
      assert(
        ScenarioLoader.load(path) ==
          Left("Criterion 'clarity' is a rubric and needs a passThreshold between 0 and 1")
      )
      Files.writeString(path, yaml + "    pass_threshold: 0.75\n")
      assert(ScenarioLoader.load(path).isRight)
    finally Files.deleteIfExists(path)
  }
