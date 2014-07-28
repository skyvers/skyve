package org.skyve.wildcat.persistence;

/**
 * Abstract Base class of Queries that can create MapBeans from their projections. 
 * We must supply the module name and document to a MapBean so it knows its document.
 */
public abstract class ProjectionQuery extends QueryImpl {
	protected String drivingModuleName;
	protected String drivingDocumentName;

	public String getDrivingModuleName() {
		return drivingModuleName;
	}

	public String getDrivingDocumentName() {
		return drivingDocumentName;
	}
}
