package modules.admin;

import java.util.ArrayList;
import java.util.List;

import modules.admin.domain.DataGroup;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.BeanVisitor;

public class DataGroupUtil {
	
	/**
	 * Iterate throughout all levels of the bean, modifying the datagroup to
	 * that specified.
	 * 
	 * @param b
	 *            - the bean to modify
	 * @param dg
	 *            - the datagroup to set
	 * @throws Exception
	 *             general Exception
	 */
	public static void publishToDataGroup(Bean b, DataGroup dg) throws Exception {

		Customer customer = CORE.getPersistence().getUser().getCustomer();
		Document document = (customer.getModule(b.getBizModule())).getDocument(customer, b.getBizDocument());

		System.out.println("For DataGroup" + dg.getName());

		new BeanVisitor(true, true, false) {
			@Override
			protected boolean accept(String binding, Document doc, Document owningDocument, Relation owningRelation, Bean bean) throws Exception {
				System.out.println("binding: " + binding);
				return true;
			}
		}.visit(document, b, customer);
	}

	public static boolean currentAdminUserIsInDataGroup(){
		return (ModulesUtil.currentAdminUser().getBizDataGroupId()!=null);		
	}
	
	/**
	 * Returns the list of data groups relevant for this customer
	 * 
	 * @param persistence
	 * @return
	 * @throws Exception
	 */
	public static List<DomainValue> getDataGroupDomainValues(Persistence persistence) throws Exception{
		
		List<DomainValue> result = new ArrayList<>();
		
		//use passed persistence if available
		Persistence pers = null;
		if(persistence==null){
			pers = CORE.getPersistence();
		} else {
			pers = persistence;
		}
		
		DocumentQuery query = pers.newDocumentQuery(DataGroup.MODULE_NAME, DataGroup.DOCUMENT_NAME);
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
	 * @param bean
	 * @return
	 * @throws Exception
	 */
	public static DataGroup getBeanDataGroup(Bean bean) throws Exception{
		DataGroup result  = null;
		
		if(bean.getBizDataGroupId()!=null){
			Persistence pers = CORE.getPersistence();
			User user = pers.getUser();
			Customer customer = user.getCustomer();
			Module module = customer.getModule(DataGroup.MODULE_NAME);
			Document document = module.getDocument(customer, DataGroup.DOCUMENT_NAME);
			
			result = pers.retrieve(document, bean.getBizDataGroupId());
		}
		
		return result;
	}
	
}
