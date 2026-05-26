package org.skyve.metadata.sail.language.step.interaction.navigation;

import org.skyve.metadata.sail.execution.Executor;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * Navigates to the map view for the module and document specified by the
 * inherited {@link NavigateList} attributes, optionally configuring geometry,
 * refresh interval, and refresh control visibility.
 *
 * @see NavigateList
 * @see NavigateCalendar
 * @see org.skyve.metadata.sail.execution.Executor#executeNavigateMap
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateMap extends NavigateList {
	private String geometryBinding;
	private Integer refreshTimeInSeconds;
	private Boolean showRefreshControls;

	/**
	 * Returns the geometryBinding.
	 * @return the result
	 */
	public String getGeometryBinding() {
		return geometryBinding;
	}
	
	/**
	 * Sets the geometryBinding.
	 * @param geometryBinding the geometryBinding
	 */
	@XmlAttribute
	public void setGeometryBinding(String geometryBinding) {
		this.geometryBinding = UtilImpl.processStringValue(geometryBinding);
	}

	/**
	 * Returns the refreshTimeInSeconds.
	 * @return the result
	 */
	public Integer getRefreshTimeInSeconds() {
		return refreshTimeInSeconds;
	}
	
	/**
	 * Sets the refreshTimeInSeconds.
	 * @param refreshTimeInSeconds the refreshTimeInSeconds
	 */
	@XmlAttribute(required = false)
	public void setRefreshTimeInSeconds(Integer refreshTimeInSeconds) {
		this.refreshTimeInSeconds = refreshTimeInSeconds;
	}
	
	/**
	 * Returns the showRefreshControls.
	 * @return the result
	 */
	public Boolean getShowRefreshControls() {
		return showRefreshControls;
	}
	
	/**
	 * Sets the showRefreshControls.
	 * @param showRefreshControls the showRefreshControls
	 */
	@XmlAttribute(required = false)
	public void setShowRefreshControls(Boolean showRefreshControls) {
		this.showRefreshControls = showRefreshControls;
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeNavigateMap(this);
	}
}
