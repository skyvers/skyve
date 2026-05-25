package org.skyve.metadata.sail.language;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.util.Util;
import org.skyve.web.UserAgentType;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Represents a complete SAIL automation script ready for execution.
 *
 * <p>An {@code Automation} is the root unit of a SAIL script file. It specifies:
 * <ul>
 *   <li>The UX/UI name that determines which rendered UI the script targets.</li>
 *   <li>The {@link UserAgentType} (phone, tablet, desktop) that constrains element location.</li>
 *   <li>The {@link TestStrategy} — {@code Assert} halts on first failure,
 *       {@code Verify} collects all failures, {@code None} runs without assertions.</li>
 *   <li>Optional {@code before} and {@code after} {@link Procedure} blocks for test setup
 *       and teardown.</li>
 *   <li>An ordered list of {@link Interaction} objects that define the test steps.</li>
 * </ul>
 *
 * <p>JAXB-bound to the SAIL XML namespace; the root element name is {@code automation}.
 * Execute via {@link org.skyve.metadata.sail.execution.Executor#executeAutomation}.
 *
 * @see Interaction
 * @see Procedure
 * @see org.skyve.metadata.sail.execution.Executor
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"uxui", "userAgentType", "testStrategy", "before", "interactions", "after"})
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Automation implements Executable {

	/**
	 * Controls how test assertion failures are handled during execution.
	 */
	@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
	public static enum TestStrategy {
		/** Stop the automation run immediately on the first assertion failure. */
		Assert,
		/** Continue running after failures and collect all assertion failures before reporting. */
		Verify,
		/** Execute all steps without performing any assertions. */
		None
	}
	
	private String uxui;
	private UserAgentType userAgentType;
	private TestStrategy testStrategy;
	private Procedure before;
	private List<Interaction> interactions = new ArrayList<>();
	private Procedure after;
	
	public String getUxui() {
		return uxui;
	}

	@XmlAttribute(name = "uxui", required = true)
	public void setUxui(String uxui) {
		this.uxui = Util.processStringValue(uxui);
	}

	public UserAgentType getUserAgentType() {
		return userAgentType;
	}

	@XmlAttribute(name = "userAgentType", required = true)
	public void setUserAgentType(UserAgentType userAgentType) {
		this.userAgentType = userAgentType;
	}

	
	public TestStrategy getTestStrategy() {
		return testStrategy;
	}

	@XmlAttribute(name = "testStrategy")
	public void setTestStrategy(TestStrategy testStrategy) {
		this.testStrategy = testStrategy;
	}

	public Procedure getBefore() {
		return before;
	}

	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "before")
	public void setBefore(Procedure before) {
		this.before = before;
	}

	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "interaction", required = true)
	public List<Interaction> getInteractions() {
		return interactions;
	}

	public Procedure getAfter() {
		return after;
	}

	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "after")
	public void setAfter(Procedure after) {
		this.after = after;
	}
	
	@Override
	public void execute(Executor executor) {
		executor.executeAutomation(this);
	}
}
