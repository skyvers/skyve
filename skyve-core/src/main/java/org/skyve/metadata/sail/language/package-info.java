/**
 * Core types of the SAIL (Skyve Automated Interface Language) DSL.
 *
 * <p>SAIL is a declarative XML-based language for describing UI automation sequences
 * against a Skyve application. The primary unit is an {@link Automation}, which
 * groups one or more named {@link Interaction} objects. Each interaction is a sequence
 * of {@link Step} objects that represent individual UI operations (navigate, enter data,
 * click buttons, assert values, etc.).
 *
 * <p>The language model is pure data — it is decoupled from execution via the
 * {@link Executor} visitor in {@code org.skyve.metadata.sail.execution}. This
 * allows the same script to be executed by different runtimes (Selenium, mock) or
 * traversed by code generators.
 *
 * <p>All SAIL classes use JAXB annotations for XML serialisation, with the namespace
 * defined by {@code XMLMetaData.SAIL_NAMESPACE}.
 *
 * @see org.skyve.metadata.sail.execution.Executor
 */
package org.skyve.metadata.sail.language;
