package org.skyve.metadata.sail.language;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.language.Automation.TestStrategy;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * A SAIL workflow: several automations in one file, run in order as one run, for example
 * <pre>
 * &lt;workflow xmlns="http://www.skyve.org/xml/sail"&gt;
 * 	&lt;automation uxui="tablet" userAgentType="tablet"&gt;...&lt;/automation&gt;
 * 	&lt;automation uxui="desktop" userAgentType="desktop"&gt;...&lt;/automation&gt;
 * &lt;/workflow&gt;
 * </pre>
 *
 * <p>Each automation keeps its own UX/UI, user agent type and test strategy, so a workflow can move between renderers,
 * for example a client in a PrimeFaces UX/UI and then staff in SmartClient. The automations share one run: the run
 * token that {@code {run}} stands for, and the variables that capture steps set for {@code {var:name}}. An automation
 * can therefore find what an earlier one made.
 *
 * <p>An optional {@code testStrategy} on the workflow is the default for its automations that state none; an
 * automation's own attribute wins (see {@link #getEffectiveTestStrategy(Automation)}).
 *
 * <p>A workflow is not an {@link Executable}: its automations can need different executors (one per renderer), so a
 * runner such as the interpreted harness's {@code util.sail.SailWorkflow} executes each automation with the right
 * executor and shares the run between them. A single {@link Automation} file is a workflow of one; see
 * {@link XMLMetaData#unmarshalSAILWorkflowString(String)}.
 *
 * @see Automation
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"testStrategy", "automations"})
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Workflow {
	private TestStrategy testStrategy;
	private List<Automation> automations = new ArrayList<>();

	/**
	 * Returns the default test strategy for the automations that state none.
	 *
	 * @return the default, or {@code null} if the workflow states none
	 */
	public @Nullable TestStrategy getTestStrategy() {
		return testStrategy;
	}

	/**
	 * Sets the default test strategy for the automations that state none.
	 *
	 * @param testStrategy	the default, or {@code null} for none
	 */
	@XmlAttribute(name = "testStrategy")
	public void setTestStrategy(@Nullable TestStrategy testStrategy) {
		this.testStrategy = testStrategy;
	}

	/**
	 * Returns the automations, in the order they run.
	 *
	 * @return the automations (a live list)
	 */
	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "automation", required = true)
	public @Nonnull List<Automation> getAutomations() {
		return automations;
	}

	/**
	 * Returns the test strategy that an automation runs with before any suite default or run override: its own, else
	 * this workflow's default.
	 *
	 * @param automation	one of this workflow's automations
	 * @return the strategy, or {@code null} if neither states one
	 * @see Suite#getEffectiveTestStrategy(Workflow, Automation)
	 */
	public @Nullable TestStrategy getEffectiveTestStrategy(@Nonnull Automation automation) {
		TestStrategy result = automation.getTestStrategy();
		return (result == null) ? testStrategy : result;
	}
}
