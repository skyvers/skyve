package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Action.ActionShow;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Abstract JAXB base for action descriptors that carry a display position.
 *
 * <p>Adds an optional {@code displayOrder} integer to {@link ActionMetaData}
 * so the action button can be placed at a specific position in the rendered
 * action bar.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ActionMetaData
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public abstract class PositionableAction extends ActionMetaData {
	private static final long serialVersionUID = -7322904477575844567L;

	private Boolean inActionPanel;
	private ActionShow show;

	/**
	 * Indicates whether this action is shown in the action panel.
	 *
	 * @return {@code true} for action-panel placement, {@code false} otherwise, or {@code null}
	 */
	public Boolean getInActionPanel() {
		return inActionPanel;
	}
	
	/**
	 * Sets whether this action is shown in the action panel.
	 *
	 * @param inActionPanel {@code true} to place in the action panel, {@code false} otherwise
	 */
	@XmlAttribute(required = false)
	public void setInActionPanel(Boolean inActionPanel) {
		this.inActionPanel = inActionPanel;
	}

	/**
	 * Returns action visibility behaviour in the rendered UI.
	 *
	 * @return visibility mode, or {@code null}
	 */
	public ActionShow getShow() {
		return show;
	}
	
	/**
	 * Sets action visibility behaviour in the rendered UI.
	 *
	 * @param show visibility mode
	 */
	@XmlAttribute(required = false)
	public void setShow(ActionShow show) {
		this.show = show;
	}

	/**
	 * Converts this descriptor to runtime metadata including action placement hints.
	 *
	 * @return runtime action metadata with action-panel and show settings applied
	 */
	@Override
	public ActionImpl toMetaDataAction() {
		ActionImpl result = super.toMetaDataAction();
		result.setInActionPanel(inActionPanel);
		result.setShow(show);
		return result;
	}
}
