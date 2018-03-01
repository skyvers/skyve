package org.skyve.impl.tools.test.sail.language;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.tools.test.sail.XMLUtil;
import org.skyve.impl.tools.test.sail.execution.Executor;
import org.skyve.impl.tools.test.sail.language.step.Execute;
import org.skyve.impl.tools.test.sail.language.step.Test;
import org.skyve.impl.tools.test.sail.language.step.interaction.DataEnter;
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
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateCalendar;
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateMap;
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateMenu;
import org.skyve.impl.tools.test.sail.language.step.interaction.navigation.NavigateTree;
import org.skyve.impl.util.UtilImpl;

@XmlType(namespace = XMLUtil.SAIL_NAMESPACE, propOrder = {"setup", "steps", "tearDown"})
@XmlRootElement(namespace = XMLUtil.SAIL_NAMESPACE)
public class TestCase implements Executable {
	private String name;
	private Procedure setup;
	private List<Step> steps = new ArrayList<>();
	private Procedure tearDown;

	public String getName() {
		return name;
	}

	@XmlAttribute(required = true)
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}
	
	public Procedure getSetup() {
		return setup;
	}

	@XmlElement(namespace = XMLUtil.SAIL_NAMESPACE, name = "setup")
	public void setSetup(Procedure setup) {
		this.setup = setup;
	}

	public Procedure getTearDown() {
		return tearDown;
	}

	@XmlElement(namespace = XMLUtil.SAIL_NAMESPACE, name = "tearDown")
	public void setTearDown(Procedure tearDown) {
		this.tearDown = tearDown;
	}

	@XmlElementWrapper(namespace = XMLUtil.SAIL_NAMESPACE, name = "method")
	@XmlElementRefs({@XmlElementRef(type = NavigateMenu.class),
						@XmlElementRef(type = NavigateList.class),
						@XmlElementRef(type = NavigateEdit.class),
						@XmlElementRef(type = NavigateTree.class),
						@XmlElementRef(type = NavigateMap.class),
						@XmlElementRef(type = NavigateCalendar.class),
						@XmlElementRef(type = NavigateLink.class),
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
						@XmlElementRef(type = Execute.class)})
	public List<Step> getSteps() {
		return steps;
	}
	
	@Override
	public void execute(Executor executor) {
		executor.execute(this);
	}
}
