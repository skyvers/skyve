package org.skyve.impl.metadata.view.reference;

public abstract class ReferenceProcessor {
	public final void process(Reference reference) {
		if (reference instanceof ActionReference) {
			processActionReference((ActionReference) reference);
		}
		else if (reference instanceof ContentReference) {
			processContentReference((ContentReference) reference);
		}
		else if (reference instanceof DefaultListViewReference) {
			processDefaultListViewReference((DefaultListViewReference) reference);
		}
		else if (reference instanceof EditViewReference) {
			processEditViewReference((EditViewReference) reference);
		}
		else if (reference instanceof ExternalReference) {
			processExternalReference((ExternalReference) reference);
		}
		else if (reference instanceof ImplicitActionReference) {
			processImplicitActionReference((ImplicitActionReference) reference);
		}
		else if (reference instanceof QueryListViewReference) {
			processQueryListViewReference((QueryListViewReference) reference);
		}
		else if (reference instanceof ReportReference) {
			processReportReference((ReportReference) reference);
		}
		else if (reference instanceof ResourceReference) {
			processResourceReference((ResourceReference) reference);
		}
		else if (reference != null) {
			throw new IllegalStateException("Reference Type " + reference.getClass() + " is not catered for");
		}
	}
	
	public abstract void processActionReference(ActionReference reference);
	public abstract void processContentReference(ContentReference reference);
	public abstract void processDefaultListViewReference(DefaultListViewReference reference);
	public abstract void processEditViewReference(EditViewReference reference);
	public abstract void processExternalReference(ExternalReference reference);
	public abstract void processImplicitActionReference(ImplicitActionReference reference);
	public abstract void processQueryListViewReference(QueryListViewReference reference);
	public abstract void processReportReference(ReportReference reference);
	public abstract void processResourceReference(ResourceReference reference);
}
