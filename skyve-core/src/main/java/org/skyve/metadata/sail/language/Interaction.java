package org.skyve.metadata.sail.language;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Execute;
import org.skyve.metadata.sail.language.step.Pause;
import org.skyve.metadata.sail.language.step.TestFailure;
import org.skyve.metadata.sail.language.step.TestSuccess;
import org.skyve.metadata.sail.language.step.TestValue;
import org.skyve.metadata.sail.language.step.context.ClearContext;
import org.skyve.metadata.sail.language.step.context.PopContext;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.DataEnter;
import org.skyve.metadata.sail.language.step.interaction.TabSelect;
import org.skyve.metadata.sail.language.step.interaction.TestDataEnter;
import org.skyve.metadata.sail.language.step.interaction.actions.Action;
import org.skyve.metadata.sail.language.step.interaction.actions.Cancel;
import org.skyve.metadata.sail.language.step.interaction.actions.Delete;
import org.skyve.metadata.sail.language.step.interaction.actions.Ok;
import org.skyve.metadata.sail.language.step.interaction.actions.Remove;
import org.skyve.metadata.sail.language.step.interaction.actions.Save;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomIn;
import org.skyve.metadata.sail.language.step.interaction.actions.ZoomOut;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridEdit;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridRemove;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.DataGridZoom;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridNew;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridSelect;
import org.skyve.metadata.sail.language.step.interaction.grids.ListGridZoom;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionEdit;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionNew;
import org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateCalendar;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMap;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Represents a named sequence of automation steps within an {@link Automation} script.
 *
 * <p>An {@code Interaction} is the primary unit of script organisation — analogous
 * to a test method. It has a {@code name} attribute (displayed in reports), optional
 * {@code before} and {@code after} {@link Procedure} hooks, and an ordered list of
 * {@link Step} objects in a {@code method} element.
 *
 * <p>Steps cover the full set of UI operations: session management (login/logout),
 * navigation (list/edit/tree/map/calendar), data entry, action buttons, grid operations,
 * lookup/description widgets, tab selection, context management, and test assertions.
 *
 * <p>JAXB-bound to the SAIL XML namespace. Execute via
 * {@link org.skyve.metadata.sail.execution.Executor#executeInteraction}.
 *
 * @see Automation
 * @see Procedure
 * @see Step
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"before", "steps", "after"})
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Interaction implements Executable {

	private String name;
	private Procedure before;
	private List<Step> steps = new ArrayList<>();
	private Procedure after;

	/**
	 * Returns the name.
	 * @return the result
	 */
	public String getName() {
		return name;
	}

	/**
	 * Sets the name.
	 * @param name the name
	 */
	@XmlAttribute(required = true)
	public void setName(String name) {
		this.name = UtilImpl.processStringValue(name);
	}
	
	/**
	 * Returns the before.
	 * @return the result
	 */
	public Procedure getBefore() {
		return before;
	}

	/**
	 * Sets the before.
	 * @param before the before
	 */
	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "before")
	public void setBefore(Procedure before) {
		this.before = before;
	}

	/**
	 * Returns the after.
	 * @return the result
	 */
	public Procedure getAfter() {
		return after;
	}

	/**
	 * Sets the after.
	 * @param after the after
	 */
	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "after")
	public void setAfter(Procedure after) {
		this.after = after;
	}

	/**
	 * Returns the steps.
	 * @return the result
	 */
	@XmlElementWrapper(namespace = XMLMetaData.SAIL_NAMESPACE, name = "method")
	@XmlElementRefs({@XmlElementRef(type = Login.class),
						@XmlElementRef(type = Logout.class),
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
						@XmlElementRef(type = ZoomIn.class),
						@XmlElementRef(type = DataGridNew.class),
						@XmlElementRef(type = DataGridZoom.class),
						@XmlElementRef(type = DataGridEdit.class),
						@XmlElementRef(type = DataGridRemove.class),
						@XmlElementRef(type = DataGridSelect.class),
						@XmlElementRef(type = ListGridNew.class),
						@XmlElementRef(type = ListGridZoom.class),
						@XmlElementRef(type = ListGridSelect.class),
						@XmlElementRef(type = TestValue.class),
						@XmlElementRef(type = TestSuccess.class),
						@XmlElementRef(type = TestFailure.class),
						@XmlElementRef(type = Comment.class),
						@XmlElementRef(type = Execute.class),
						@XmlElementRef(type = Pause.class),
						@XmlElementRef(type = PushListContext.class),
						@XmlElementRef(type = PushEditContext.class),
						@XmlElementRef(type = PopContext.class),
						@XmlElementRef(type = ClearContext.class)})
	public List<Step> getSteps() {
		return steps;
	}
	
	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeInteraction(this);
	}
}
