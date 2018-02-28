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
import org.skyve.impl.tools.test.sail.language.step.Selenese;
import org.skyve.impl.tools.test.sail.language.step.Test;
import org.skyve.impl.tools.test.sail.language.step.interaction.DataEnter;
import org.skyve.impl.tools.test.sail.language.step.interaction.Menu;
import org.skyve.impl.tools.test.sail.language.step.interaction.TabSelect;
import org.skyve.impl.tools.test.sail.language.step.interaction.TestDataEnter;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.Action;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.Cancel;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.Delete;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.Ok;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.Remove;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.Save;
import org.skyve.impl.tools.test.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.DataGridEdit;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.DataGridRemove;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.DataGridSelect;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.DataGridZoom;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.ListGridSelect;
import org.skyve.impl.tools.test.sail.language.step.interaction.grids.ListGridZoom;
import org.skyve.impl.tools.test.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.impl.tools.test.sail.language.step.interaction.lookup.LookupDescriptionEdit;
import org.skyve.impl.tools.test.sail.language.step.interaction.lookup.LookupDescriptionNew;
import org.skyve.impl.tools.test.sail.language.step.interaction.lookup.LookupDescriptionPick;
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
	@XmlElementRefs({@XmlElementRef(type = Menu.class),
						@XmlElementRef(type = TabSelect.class),
						@XmlElementRef(type = TestDataEnter.class),
						@XmlElementRef(type = DataEnter.class),
						@XmlElementRef(type = Ok.class),
						@XmlElementRef(type = Save.class),
						@XmlElementRef(type = Cancel.class),
						@XmlElementRef(type = Delete.class),
						@XmlElementRef(type = ZoomOut.class),
						@XmlElementRef(type = Remove.class),
						@XmlElementRef(type = Action.class),
						@XmlElementRef(type = LookupDescriptionAutoComplete.class),
						@XmlElementRef(type = LookupDescriptionPick.class),
						@XmlElementRef(type = LookupDescriptionNew.class),
						@XmlElementRef(type = LookupDescriptionEdit.class),
						@XmlElementRef(type = DataGridNew.class),
						@XmlElementRef(type = DataGridZoom.class),
						@XmlElementRef(type = DataGridEdit.class),
						@XmlElementRef(type = DataGridRemove.class),
						@XmlElementRef(type = DataGridSelect.class),
						@XmlElementRef(type = ListGridZoom.class),
						@XmlElementRef(type = ListGridSelect.class),
						@XmlElementRef(type = Test.class),
						@XmlElementRef(type = Selenese.class)})
	@Override
	public List<Step> getSteps() {
		return super.getSteps();
	}
	
	@Override
	public void execute(StringBuilder script, int indentationDepth) {
		startTest(identifier, script, indentationDepth);
		if (setup != null) {
			setup.execute(script, indentationDepth);
		}
		super.execute(script, indentationDepth);
		if (tearDown != null) {
			tearDown.execute(script, indentationDepth);
		}
		endTest(script, indentationDepth);
	}
}
