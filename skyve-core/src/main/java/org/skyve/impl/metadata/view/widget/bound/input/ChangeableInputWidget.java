package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Abstract JAXB base for input widgets that support value-change events.
 *
 * <p>Extends {@link FocusableInputWidget} with an onChange event handler list.
 * Most concrete input controls extend this class.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"changedActions"})
public abstract class ChangeableInputWidget extends FocusableInputWidget implements Changeable {
	private static final long serialVersionUID = 8974421976409722475L;

	private List<EventAction> changedActions = new ArrayList<>();
	
	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "onChangedHandlers")
	@XmlElementRefs({@XmlElementRef(type = RerenderEventAction.class), 
						@XmlElementRef(type = ServerSideActionEventAction.class),
						@XmlElementRef(type = SetDisabledEventAction.class),
						@XmlElementRef(type = SetInvisibleEventAction.class),
						@XmlElementRef(type = ToggleDisabledEventAction.class),
						@XmlElementRef(type = ToggleVisibilityEventAction.class)})
	public List<EventAction> getChangedActions() {
		return changedActions;
	}
}
