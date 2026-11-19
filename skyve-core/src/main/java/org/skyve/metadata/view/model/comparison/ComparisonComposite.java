package org.skyve.metadata.view.model.comparison;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.document.Document;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class ComparisonComposite {
	@SuppressWarnings("java:S115") // Enum names are comparison state values exposed to callers.
	public enum Mutation {
		unchanged,
		added,
		updated,
		deleted
	}
	
	private String bizId;
	private String businessKeyDescription;
	private String referenceName;
	private String relationshipDescription;
	private Mutation mutation;
	private Document document;
	private List<ComparisonProperty> properties = new ArrayList<>();
	private List<ComparisonComposite> children = new ArrayList<>();

	public ComparisonComposite() {
	}
	
	public ComparisonComposite(@Nullable String bizId,
								@Nullable String businessKeyDescription,
								@Nullable String referenceName,
								@Nullable String relationshipDescription,
								@Nullable Mutation mutation,
								@Nullable Document document) {
		this.bizId = bizId;
		this.businessKeyDescription = businessKeyDescription;
		this.referenceName = referenceName;
		this.relationshipDescription = relationshipDescription;
		this.mutation = mutation;
		this.document = document;
	}


	public @Nullable String getBizId() {
		return bizId;
	}
	public void setBizId(@Nullable String bizId) {
		this.bizId = bizId;
	}
	public @Nullable String getBusinessKeyDescription() {
		return businessKeyDescription;
	}
	public void setBusinessKeyDescription(@Nullable String businessKeyDescription) {
		this.businessKeyDescription = businessKeyDescription;
	}
	public @Nullable String getReferenceName() {
		return referenceName;
	}
	public void setReferenceName(@Nullable String referenceName) {
		this.referenceName = referenceName;
	}
	public @Nullable String getRelationshipDescription() {
		return relationshipDescription;
	}
	public void setRelationshipDescription(@Nullable String relationshipDescription) {
		this.relationshipDescription = relationshipDescription;
	}
	public @Nullable Mutation getMutation() {
		return mutation;
	}
	public void setMutation(@Nullable Mutation mutation) {
		this.mutation = mutation;
	}
	public @Nullable Document getDocument() {
		return document;
	}
	public void setDocument(@Nullable Document document) {
		this.document = document;
	}
	public @Nonnull List<ComparisonProperty> getProperties() {
		return properties;
	}
	public @Nonnull List<ComparisonComposite> getChildren() {
		return children;
	}
	
	public void determineMutations() {
		determineMutation();
	}

	private boolean determineMutation() {
		boolean dirty = (mutation != null) && (! Mutation.unchanged.equals(mutation));

		for (ComparisonComposite child : children) {
			dirty = child.determineMutation() || dirty; // ensure function is called and not short-circuited
		}
		
		if (! dirty) {
			// is this dirty?
			for (ComparisonProperty property : properties) {
				if (property.isDirty()) {
					dirty = true;
					break;
				}
			}
		}

		if (dirty && ((mutation == null) || Mutation.unchanged.equals(mutation))) {
			mutation = Mutation.updated;
		}
		
		return dirty;
	}
}
