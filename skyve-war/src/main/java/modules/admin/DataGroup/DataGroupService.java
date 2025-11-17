package modules.admin.DataGroup;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.NullableBeanVisitor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.enterprise.inject.Default;
import jakarta.inject.Inject;
import modules.admin.domain.DataGroup;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient DataGroupService dataGroupService;
 */
@Default
public class DataGroupService {
	private static final Logger LOGGER = LoggerFactory.getLogger(DataGroupService.class);
	@Inject
	private transient Persistence pers;

	/**
	 * Iterate throughout all levels of the bean, modifying the datagroup to
	 * that specified.
	 * 
	 * @param b
	 *        - the bean to modify
	 * @param dg
	 *        - the datagroup to set
	 * @throws Exception
	 *         general Exception
	 */
	public void publishToDataGroup(Bean b, DataGroup dg) throws Exception {

		Customer customer = pers.getUser().getCustomer();
		Document document = (customer.getModule(b.getBizModule())).getDocument(customer, b.getBizDocument());

		LOGGER.debug("For DataGroup {}", dg.getName());

		new NullableBeanVisitor(true, false) {
			@Override
			protected boolean acceptNulls(String binding, Document doc, Document owningDocument, Relation owningRelation, Bean bean)
					throws Exception {
				LOGGER.debug("binding: {}", binding);
				return true;
			}
		}.visit(document, b, customer);
	}

	/**
	 * Returns the list of data groups relevant for this customer
	 * 
	 * @param persistence the persistence context to use for database operations, or null to use the injected persistence
	 * @return a list of DomainValue objects representing the data groups, ordered by name ascending
	 * @throws Exception if an error occurs during database query or data retrieval
	 */
	public List<DomainValue> getDataGroupDomainValues(Persistence persistence) throws Exception {

		List<DomainValue> result = new ArrayList<>();

		// use passed persistence if available
		Persistence p = persistence;
		if (p == null) {
			p = pers;
		}

		DocumentQuery query = p.newDocumentQuery(DataGroup.MODULE_NAME, DataGroup.DOCUMENT_NAME);
		query.addBoundOrdering(DataGroup.namePropertyName, SortDirection.ascending);
		List<DataGroup> groups = query.beanResults();
		for (DataGroup group : groups) {
			result.add(new DomainValue(group.getBizId(), group.getBizKey()));
		}

		return result;
	}

	/**
	 * retrieve the data group for a given bean (from the BizDataGroupId)
	 * 
	 * @param bean the bean object containing the BizDataGroupId to retrieve the data group for
	 * @return the DataGroup object associated with the bean, or null if the bean has no BizDataGroupId
	 * @throws Exception if an error occurs during data group retrieval or document access
	 */
	public DataGroup getBeanDataGroup(Bean bean) throws Exception {
		DataGroup result = null;

		if (bean.getBizDataGroupId() != null) {
			result = pers.retrieve(DataGroup.MODULE_NAME, DataGroup.DOCUMENT_NAME, bean.getBizDataGroupId());
		}

		return result;
	}
}
