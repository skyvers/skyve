package org.skyve.metadata.sail.language;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;

@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"setup", "cases", "tearDown"})
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TestSuite implements Executable {
	private Procedure setup;
	private List<TestCase> cases = new ArrayList<>();
	private Procedure tearDown;
	
	public Procedure getSetup() {
		return setup;
	}

	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "setup")
	public void setSetup(Procedure setup) {
		this.setup = setup;
	}

	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "testCase", required = true)
	public List<TestCase> getCases() {
		return cases;
	}

	public Procedure getTearDown() {
		return tearDown;
	}

	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "tearDown")
	public void setTearDown(Procedure tearDown) {
		this.tearDown = tearDown;
	}
	
	@Override
	public void execute(Executor executor) {
		executor.execute(this);
	}
}
