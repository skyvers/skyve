package org.skyve.metadata.sail.language;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Execute;
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

import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Procedure {
	private List<Step> steps = new ArrayList<>();

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
						@XmlElementRef(type = PushListContext.class),
						@XmlElementRef(type = PushEditContext.class),
						@XmlElementRef(type = PopContext.class),
						@XmlElementRef(type = ClearContext.class)})
	public List<Step> getSteps() {
		return steps;
	}
}
