package org.skyve.metadata.view.model.map;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.types.Timestamp;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public final class MapItem {
	private String bizId;
	public @Nullable String getBizId() {
		return bizId;
	}
	public void setBizId(@Nullable String bizId) {
		this.bizId = bizId;
	}

	private String moduleName;
	public @Nullable String getModuleName() {
		return moduleName;
	}
	public void setModuleName(@Nullable String moduleName) {
		this.moduleName = moduleName;
	}

	private String documentName;
	public @Nullable String getDocumentName() {
		return documentName;
	}
	public void setDocumentName(@Nullable String documentName) {
		this.documentName = documentName;
	}

	private String infoMarkup;
	public @Nullable String getInfoMarkup() {
		return infoMarkup;
	}
	public void setInfoMarkup(@Nullable String infoMarkup) {
		this.infoMarkup = infoMarkup;
	}

	private Timestamp fromTimestamp;
	public @Nullable Timestamp getFromTimestamp() {
		return fromTimestamp;
	}
	public void setFromTimestamp(@Nullable Timestamp fromTimestamp) {
		this.fromTimestamp = fromTimestamp;
	}
	
	private Timestamp toTimestamp;
	public @Nullable Timestamp getToTimestamp() {
		return toTimestamp;
	}
	public void setToTimestamp(@Nullable Timestamp toTimestamp) {
		this.toTimestamp = toTimestamp;
	}
	
	private List<MapFeature> features = new ArrayList<>();
	public @Nonnull List<MapFeature> getFeatures() {
		return features;
	}
}
