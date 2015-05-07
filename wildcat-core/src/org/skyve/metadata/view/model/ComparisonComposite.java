package org.skyve.metadata.view.model;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.document.Document;

public class ComparisonComposite {
	public static enum Mutation {
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
	
	public ComparisonComposite(String bizId,
								String businessKeyDescription,
								String referenceName,
								Mutation mutation,
								String relationshipDescription) {
		this.bizId = bizId;
		this.businessKeyDescription = businessKeyDescription;
		this.referenceName = referenceName;
		this.mutation = mutation;
		this.relationshipDescription = relationshipDescription;
	}


	public String getBizId() {
		return bizId;
	}
	public void setBizId(String bizId) {
		this.bizId = bizId;
	}
	public String getBusinessKeyDescription() {
		return businessKeyDescription;
	}
	public void setBusinessKeyDescription(String businessKeyDescription) {
		this.businessKeyDescription = businessKeyDescription;
	}
	public String getReferenceName() {
		return referenceName;
	}
	public void setReferenceName(String referenceName) {
		this.referenceName = referenceName;
	}
	public String getRelationshipDescription() {
		return relationshipDescription;
	}
	public void setRelationshipDescription(String relationshipDescription) {
		this.relationshipDescription = relationshipDescription;
	}
	public Mutation getMutation() {
		return mutation;
	}
	public void setMutation(Mutation mutation) {
		this.mutation = mutation;
	}
	public Document getDocument() {
		return document;
	}
	public void setDocument(Document document) {
		this.document = document;
	}
	public List<ComparisonProperty> getProperties() {
		return properties;
	}
	public List<ComparisonComposite> getChildren() {
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
