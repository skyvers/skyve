package org.skyve.impl.web.faces.actions;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.FacesException;

/**
 * Just set the state of the list bean up - but do not load any data
 */
public class PopulateAction extends FacesAction<Void> {
    private static final Logger FACES_LOGGER = Category.FACES.logger();

	private FacesView facesView;

	/**
	 * Creates an action that initialises list-view state without loading data rows.
	 *
	 * @param facesView the current Faces view context
	 */
	public PopulateAction(@Nonnull FacesView facesView) {
		this.facesView = facesView;
	}
	
	/**
	 * Initialises the list view metadata state for the current request.
	 *
	 * <p>If both document and query parameters are present, this resolves the list model
	 * from the document and applies its localised description to the view title. Otherwise,
	 * it resolves the metadata query definition and derives the document name and title
	 * from that definition.
	 *
	 * @return always {@code null} because this action mutates view state only
	 * @throws Exception if metadata resolution fails while loading module, document, or query details
	 */
	@Override
	public @Nullable Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) FACES_LOGGER.info("PopulateAction");

		String bizModule = facesView.getBizModuleParameter();
		String bizDocument = facesView.getBizDocumentParameter();
		String queryName = facesView.getQueryNameParameter();
		if (bizModule == null) { // HarnessView.initialise() defaults the module
			throw new IllegalStateException("Cannot populate a list as the FacesView has no module parameter");
		}
		if (queryName == null) {
			throw new FacesException("Malformed URL - a list of module " + bizModule + " requires a query name");
		}
		if (bizDocument != null) {
			Customer c = CORE.getCustomer();
			Module m = c.getModule(bizModule);
			Document d = m.getDocument(c, bizDocument);
			ListModel<Bean> lm = d.getListModel(c, queryName, true);
			facesView.setTitle(lm.getLocalisedDescription());
			Document drivingDocument = lm.getDrivingDocument();
			if (drivingDocument == null) {
				throw new IllegalStateException("List model " + queryName + " of document " + bizModule + '.' + bizDocument + " has no driving document");
			}
			facesView.setTitleIcon(drivingDocument, null);
			facesView.setModelName(queryName);
			facesView.setQueryNameParameter(null);
		}
		else {
			MetaDataQueryDefinition query = ActionUtil.getMetaDataQuery(bizModule, queryName);
			String documentName = query.getDocumentName();
			facesView.setBizDocumentParameter(documentName);
			facesView.setTitle(query.getLocalisedDescription());
			Customer customer = CORE.getCustomer();
			Module documentModule = query.getDocumentModule(customer);
			Document document = documentModule.getDocument(customer, documentName);
			facesView.setTitleIcon(document, null);
		}
		
		return null;
	}
}
