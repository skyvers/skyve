package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.util.XMLUtil;
import org.skyve.impl.metadata.view.widget.bound.input.FocusableInputWidget;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"changedActions"})
public abstract class ChangeableInputWidget extends FocusableInputWidget implements Changeable {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 8974421976409722475L;

	private List<EventAction> changedActions = new ArrayList<>();
	
	@Override
	@XmlElementWrapper(namespace = XMLUtil.VIEW_NAMESPACE, name = "onChangedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class),
						@XmlElementRef(type = ToggleDisabledEventAction.class)})
	public List<EventAction> getChangedActions() {
		return changedActions;
	}
}
