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

package com.salesforce.spearhead.evalon

import scala.jdk.CollectionConverters.*

/** Thrown by {@link EvalonRunner#run} before the simulation starts when the scenario fails
  * {@code ScenarioValidator}, so no LLM call is made. Retrying cannot help; the scenario must be
  * fixed.
  *
  * @param errors one human-readable problem per invalid criterion rule
  */
final class InvalidScenarioException(errors: List[String])
    extends RuntimeException(errors.mkString("; ")):

  def this(errors: java.util.List[String]) = this(errors.asScala.toList)

  def getErrors: java.util.List[String] = errors.asJava
