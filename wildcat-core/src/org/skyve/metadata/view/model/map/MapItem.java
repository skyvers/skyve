package org.skyve.metadata.view.model.map;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.types.Timestamp;

public final class MapItem {
	private String bizId;
	public String getBizId() {
		return bizId;
	}
	public void setBizId(String bizId) {
		this.bizId = bizId;
	}

	private String moduleName;
	public String getModuleName() {
		return moduleName;
	}
	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}

	private String documentName;
	public String getDocumentName() {
		return documentName;
	}
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}

	private String infoMarkup;
	public String getInfoMarkup() {
		return infoMarkup;
	}
	public void setInfoMarkup(String infoMarkup) {
		this.infoMarkup = infoMarkup;
	}

	private Timestamp fromTimestamp;
	public Timestamp getFromTimestamp() {
		return fromTimestamp;
	}
	public void setFromTimestamp(Timestamp fromTimestamp) {
		this.fromTimestamp = fromTimestamp;
	}
	
	private Timestamp toTimestamp;
	public Timestamp getToTimestamp() {
		return toTimestamp;
	}
	public void setToTimestamp(Timestamp toTimestamp) {
		this.toTimestamp = toTimestamp;
	}
	
	private List<MapFeature> features = new ArrayList<>();
	public List<MapFeature> getFeatures() {
		return features;
	}
}
