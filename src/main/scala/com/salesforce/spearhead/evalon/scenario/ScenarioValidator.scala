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

import com.salesforce.spearhead.evalon.model.*

object ScenarioValidator:

  /** Human-readable problems with the scenario's criteria; empty when the scenario is valid.
    *
    * Only rubric criteria are checked: their levels live in the description, so it must be present.
    */
  def validate(scenario: Scenario): List[String] =
    scenario.evalCriteria.zipWithIndex.flatMap { (criterion, index) =>
      if criterion.criterionType != CriterionType.Rubric then Nil
      else
        val label = Option(criterion.name).filter(_.trim.nonEmpty).getOrElse(s"#${index + 1}")
        val hasDescription = Option(criterion.description).exists(_.trim.nonEmpty)
        Option.unless(hasDescription)(
          s"Criterion '$label' is a rubric and needs a description with its levels"
        ).toList
    }
