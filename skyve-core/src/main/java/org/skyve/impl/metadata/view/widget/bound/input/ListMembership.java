package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.impl.metadata.view.widget.bound.input.MembershipWidget;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"changedActions", "membersHeading", "candidatesHeading", "pixelWidth", "properties"})
public class ListMembership extends InputWidget implements MembershipWidget, AbsoluteWidth {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -2470035788382174503L;

	private Integer pixelWidth;
	private String candidatesHeading = "Candidates";
	private String membersHeading = "Members";
	private List<EventAction> changedActions = new ArrayList<>();
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

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

	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	@Override
	@XmlAttribute(name = "pixelWidth", required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	public String getCandidatesHeading() {
		return candidatesHeading;
	}

	@XmlAttribute(required = false)
	public void setCandidatesHeading(String candidatesHeading) {
		this.candidatesHeading = UtilImpl.processStringValue(candidatesHeading);
	}

	public String getMembersHeading() {
		return membersHeading;
	}

	@XmlAttribute(required = false)
	public void setMembersHeading(String membersHeading) {
		this.membersHeading = UtilImpl.processStringValue(membersHeading);
	}

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
