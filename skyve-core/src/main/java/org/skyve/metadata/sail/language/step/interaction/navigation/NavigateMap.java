package org.skyve.metadata.sail.language.step.interaction.navigation;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.sail.execution.Executor;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

/**
 * Navigate to a map view.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateMap extends NavigateList {
	private String geometryBinding;
	private Integer refreshTimeInSeconds;
	private Boolean showRefreshControls;

	public String getGeometryBinding() {
		return geometryBinding;
	}
	
	@XmlAttribute
	public void setGeometryBinding(String geometryBinding) {
		this.geometryBinding = UtilImpl.processStringValue(geometryBinding);
	}

	public Integer getRefreshTimeInSeconds() {
		return refreshTimeInSeconds;
	}
	
	@XmlAttribute(required = false)
	public void setRefreshTimeInSeconds(Integer refreshTimeInSeconds) {
		this.refreshTimeInSeconds = refreshTimeInSeconds;
	}
	
	public Boolean getShowRefreshControls() {
		return showRefreshControls;
	}
	
	@XmlAttribute(required = false)
	public void setShowRefreshControls(Boolean showRefreshControls) {
		this.showRefreshControls = showRefreshControls;
	}

	@Override
	public void execute(Executor executor) {
		executor.executeNavigateMap(this);
	}
}
