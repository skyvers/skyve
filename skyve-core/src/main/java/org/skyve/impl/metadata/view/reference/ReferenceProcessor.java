package org.skyve.impl.metadata.view.reference;

import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.UserAgentType;

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
	
	public static final Action obtainActionForActionReference(ActionReference reference,
																Customer customer,
																Module module,
																Document document,
																String dataWidgetBinding,
																UserAgentType userAgentType) {
		final TargetMetaData listTarget = BindUtil.getMetaDataForBinding(customer, module, document, dataWidgetBinding);

		final Document listDocument;
		// Figure out the document type of the relation.
		if (listTarget.getAttribute() instanceof Relation) {
			final String documentName = ((Relation) listTarget.getAttribute()).getDocumentName();
			listDocument = module.getDocument(customer, documentName);
		}
		else {
			listDocument = listTarget.getDocument();
		}

		final ViewType[] viewTypesToSearch = new ViewType[] { ViewType.edit, ViewType.create };
		Action result = null;
		for (ViewType viewType : viewTypesToSearch) {
			final View listDocumentView = listDocument.getView(userAgentType.name(), customer, viewType.name());
			if (listDocumentView == null) {
				continue;
			}
			result = listDocumentView.getAction(reference.getActionName());
			if (result != null) {
				// Found the action, we can stop looking.
				break;
			}
		}

		return result;
	}
}
