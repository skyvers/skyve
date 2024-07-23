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

@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"uxui", "userAgentType", "testStrategy", "before", "interactions", "after"})
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Automation implements Executable {
	@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
	public static enum TestStrategy {
		Assert,
		Verify,
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
