package org.skyve.wildcat.metadata.view.widget.bound.input;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.view.event.EventAction;
import org.skyve.wildcat.metadata.view.event.RerenderEventAction;
import org.skyve.wildcat.metadata.view.event.ServerSideActionEventAction;
import org.skyve.wildcat.metadata.view.event.SetDisabledEventAction;
import org.skyve.wildcat.metadata.view.event.SetInvisibleEventAction;
import org.skyve.wildcat.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE,
			propOrder = {"changedActions", "membersHeading", "candidatesHeading", "listWidthInPixels"})
public class ListMembership extends InputWidget implements MembershipWidget {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -2470035788382174503L;

	private Integer listWidthInPixels;
	private String candidatesHeading = "Candidates";
	private String membersHeading = "Members";
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

	public Integer getListWidthInPixels() {
		return listWidthInPixels;
	}

	@XmlAttribute(name = "listWidth", required = false)
	public void setListWidthInPixels(Integer listWidthInPixels) {
		this.listWidthInPixels = listWidthInPixels;
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
}
