package org.skyve.wildcat.tools.test.wail.language;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.tools.test.wail.XMLUtil;

@XmlType(namespace = XMLUtil.WAIL_NAMESPACE, propOrder = {"setup", "cases", "tearDown"})
@XmlRootElement(namespace = XMLUtil.WAIL_NAMESPACE)
public class TestSuite implements Executable {
	private Setup setup;
	private List<TestCase> cases = new ArrayList<>();
	private TearDown tearDown;
	
	public Setup getSetup() {
		return setup;
	}

	@XmlElement(namespace = XMLUtil.WAIL_NAMESPACE, name = "setup")
	public void setSetup(Setup setup) {
		this.setup = setup;
	}

	public TearDown getTearDown() {
		return tearDown;
	}

	@XmlElement(namespace = XMLUtil.WAIL_NAMESPACE, name = "tearDown")
	public void setTearDown(TearDown tearDown) {
		this.tearDown = tearDown;
	}

	@XmlElement(namespace = XMLUtil.WAIL_NAMESPACE, name = "testCase", required = true)
	public List<TestCase> getCases() {
		return cases;
	}

	@Override
	public void execute(StringBuilder script) {
		if (setup != null) {
			setup.execute(script);
		}
		for (TestCase testCase : cases) {
			testCase.execute(script);
		}
		if (tearDown != null) {
			tearDown.execute(script);
		}
	}
}
