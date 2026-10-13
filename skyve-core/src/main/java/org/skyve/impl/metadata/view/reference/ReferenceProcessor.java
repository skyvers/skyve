package org.skyve.impl.metadata.view.reference;

import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Abstract dispatcher that processes concrete {@link Reference} subtypes.
 *
 * <p>Routes each reference instance to a specialised {@code process*} hook
 * method that subclasses implement.  This centralises reference-type
 * dispatch used by view conversion logic.
 *
 * <p>Threading: not thread-safe; one processor instance per conversion run.
 */
public abstract class ReferenceProcessor {
	public final void process(@Nullable Reference reference) {
		if (reference instanceof ActionReference actionReference) {
			processActionReference(actionReference);
		}
		else if (reference instanceof ContentReference contentReference) {
			processContentReference(contentReference);
		}
		else if (reference instanceof DefaultListViewReference defaultListViewReference) {
			processDefaultListViewReference(defaultListViewReference);
		}
		else if (reference instanceof EditViewReference editViewReference) {
			processEditViewReference(editViewReference);
		}
		else if (reference instanceof ExternalReference externalReference) {
			processExternalReference(externalReference);
		}
		else if (reference instanceof ImplicitActionReference implicitActionReference) {
			processImplicitActionReference(implicitActionReference);
		}
		else if (reference instanceof QueryListViewReference queryListViewReference) {
			processQueryListViewReference(queryListViewReference);
		}
		else if (reference instanceof ReportReference reportReference) {
			processReportReference(reportReference);
		}
		else if (reference instanceof ResourceReference resourceReference) {
			processResourceReference(resourceReference);
		}
		else if (reference != null) {
			throw new IllegalStateException("Reference Type " + reference.getClass() + " is not catered for");
		}
	}
	
	public abstract void processActionReference(@Nonnull ActionReference reference);
	public abstract void processContentReference(@Nonnull ContentReference reference);
	public abstract void processDefaultListViewReference(@Nonnull DefaultListViewReference reference);
	public abstract void processEditViewReference(@Nonnull EditViewReference reference);
	public abstract void processExternalReference(@Nonnull ExternalReference reference);
	public abstract void processImplicitActionReference(@Nonnull ImplicitActionReference reference);
	public abstract void processQueryListViewReference(@Nonnull QueryListViewReference reference);
	public abstract void processReportReference(@Nonnull ReportReference reference);
	public abstract void processResourceReference(@Nonnull ResourceReference reference);
	
	public static final @Nullable Action obtainActionForActionReference(@Nonnull ActionReference reference,
																@Nullable Customer customer,
																@Nonnull Module module,
																@Nonnull Document document,
																@Nonnull String dataWidgetBinding,
																@Nonnull UserAgentType userAgentType) {
		final TargetMetaData listTarget = BindUtil.getMetaDataForBinding(customer, module, document, dataWidgetBinding);

		final Document listDocument;
		// Figure out the document type of the relation.
		Attribute attribute = listTarget.getAttribute();
		if (attribute instanceof Relation relation) {
			final String documentName = relation.getDocumentName();
			listDocument = module.getDocument(customer, documentName);
		}
		else {
			listDocument = listTarget.getDocument();
		}

		final ViewType[] viewTypesToSearch = new ViewType[] { ViewType.edit, ViewType.create };
		Action result = null;
		for (ViewType viewType : viewTypesToSearch) {
			try {
				final View listDocumentView = listDocument.getView(userAgentType.name(), customer, viewType.name());
				result = listDocumentView.getAction(reference.getActionName());
				if (result != null) {
					// Found the action, we can stop looking.
					break;
				}
			}
			catch (@SuppressWarnings("unused") MetaDataException e) {
				// No view of this type, skip to the next.
			}
		}

		return result;
	}
}
