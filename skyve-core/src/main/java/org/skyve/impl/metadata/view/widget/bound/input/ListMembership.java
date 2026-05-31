package org.skyve.impl.metadata.view.widget.bound.input;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.MinimumHeight;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated dual-list membership widget that lets the user move values
 * between a candidates list and a members list.
 *
 * <p>Bound to a collection attribute.  Displays candidate heading, members
 * heading, configurable width and minimum height, and change events.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see CheckMembership
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"changedActions",
							"membersHeading",
							"candidatesHeading",
							"pixelWidth",
							"minPixelHeight",
							"properties"})
public class ListMembership extends InputWidget implements MembershipWidget, AbsoluteWidth, MinimumHeight {
	private static final long serialVersionUID = -2470035788382174503L;

	private Integer pixelWidth;
	private Integer minPixelHeight;
	private String candidatesHeading = "Candidates";
	private String membersHeading = "Members";
	private List<EventAction> changedActions = new ArrayList<>();
	
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns the mutable handlers fired after membership values change.
	 */
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

	/**
	 * Returns the absolute pixel width, or {@code null} when renderer defaults apply.
	 */
	@Override
	public Integer getPixelWidth() {
		return pixelWidth;
	}

	/**
	 * Sets the absolute pixel width for the dual-list widget.
	 */
	@Override
	@XmlAttribute(name = "pixelWidth", required = false)
	public void setPixelWidth(Integer pixelWidth) {
		this.pixelWidth = pixelWidth;
	}

	/**
	 * Returns the minimum pixel height constraint, or {@code null} when unconstrained.
	 */
	@Override
	public Integer getMinPixelHeight() {
		return minPixelHeight;
	}

	/**
	 * Sets the minimum pixel height constraint.
	 */
	@Override
	@XmlAttribute(name = "minPixelHeight", required = false)
	public void setMinPixelHeight(Integer minPixelHeight) {
		this.minPixelHeight = minPixelHeight;
	}

	/**
	 * Returns the configured heading text for the candidate list.
	 */
	public String getCandidatesHeading() {
		return candidatesHeading;
	}

	/**
	 * Returns the candidates heading resolved through the i18n message source.
	 *
	 * @return the localised candidates heading
	 */
	public String getLocalisedCandidatesHeading() {
		return Util.i18n(candidatesHeading);
	}
	
	/**
	 * Sets the candidate-list heading after trimming and empty-string normalisation.
	 */
	@XmlAttribute(required = false)
	public void setCandidatesHeading(String candidatesHeading) {
		this.candidatesHeading = UtilImpl.processStringValue(candidatesHeading);
	}

	/**
	 * Returns the configured heading text for the members list.
	 */
	public String getMembersHeading() {
		return membersHeading;
	}

	/**
	 * Returns the members heading resolved through the i18n message source.
	 *
	 * @return the localised members heading
	 */
	public String getLocalisedMembersHeading() {
		return Util.i18n(membersHeading);
	}
	
	/**
	 * Sets the members-list heading after trimming and empty-string normalisation.
	 */
	@XmlAttribute(required = false)
	public void setMembersHeading(String membersHeading) {
		this.membersHeading = UtilImpl.processStringValue(membersHeading);
	}

	/**
	 * Returns the mutable decorator property map for this widget.
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
