package org.skyve.impl.tools.test.sail.language;

import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.language.step.Call;
import org.skyve.impl.tools.test.sail.language.step.interaction.Action;
import org.skyve.impl.tools.test.sail.language.step.interaction.Menu;
import org.skyve.impl.tools.test.sail.language.step.interaction.Module;
import org.skyve.impl.tools.test.sail.language.step.interaction.browser.Navigate;
import org.skyve.impl.tools.test.sail.language.step.interaction.browser.Reload;
import org.skyve.impl.util.UtilImpl;

@XmlType(namespace = XMLUtil.SAIL_NAMESPACE, propOrder = {"setup", "steps", "tearDown"})
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class TestCase extends Procedure {
	private String identifier;
	private Setup setup;
	private TearDown tearDown;

	public String getIdentifier() {
		return identifier;
	}

	@XmlAttribute(required = true)
	public void setIdentifier(String identifier) {
		this.identifier = UtilImpl.processStringValue(identifier);
	}
	
	public Setup getSetup() {
		return setup;
	}

	@XmlElement(namespace = XMLUtil.SAIL_NAMESPACE, name = "setup")
	public void setSetup(Setup setup) {
		this.setup = setup;
	}

	public TearDown getTearDown() {
		return tearDown;
	}

	@XmlElement(namespace = XMLUtil.SAIL_NAMESPACE, name = "tearDown")
	public void setTearDown(TearDown tearDown) {
		this.tearDown = tearDown;
	}

	@XmlElementWrapper(namespace = XMLUtil.SAIL_NAMESPACE, name = "method")
	@XmlElementRefs({@XmlElementRef(type = Call.class),
						@XmlElementRef(type = Module.class),
						@XmlElementRef(type = Action.class),
						@XmlElementRef(type = Menu.class),
						@XmlElementRef(type = Navigate.class),
						@XmlElementRef(type = Reload.class)})
	@Override
	public List<Step> getSteps() {
		return super.getSteps();
	}
	
	@Override
	public void execute(StringBuilder script) {
		if (setup != null) {
			setup.execute(script);
		}
		super.execute(script);
		if (tearDown != null) {
			tearDown.execute(script);
		}
	}
}
