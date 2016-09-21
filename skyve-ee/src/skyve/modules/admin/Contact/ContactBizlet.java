package modules.admin.Contact;

import java.util.List;

import modules.ModulesUtil;
import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.admin.domain.DataGroup;
import modules.admin.domain.User;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.web.WebContext;

public class ContactBizlet extends Bizlet<Contact> {
	private static final long serialVersionUID = 6794069388827546373L;

	@Override
	public Contact preExecute(ImplicitActionName actionName,
								Contact bean,
								Bean parentBean,
								WebContext webContext)
	throws Exception {
		if(ImplicitActionName.Add.equals(actionName) ){
			//TODO add default image using resource UnknownContact
		}
		
		return bean;
	}

	public static String bizKey(Contact bean) {
	  	StringBuilder result = new StringBuilder(64);
	  	
	  	String name = bean.getName();
		result.append((name == null) ? "Unnamed Contact" : name);
  		
		ContactType type = bean.getContactType();
  		if (type != null) {
			result.append(" (").append(type).append(')');
		}
		
		String mobile = bean.getMobile();
		if (mobile != null) {
			result.append(" (m) ").append(mobile);
		}
		
		return result.toString();
	}
	
	/**
	 * Returns true if the contact holds no data and can be dispensed with.
	 * 
	 * @param c
	 */
	public static boolean isNothing(Contact c){
		boolean result = true;
		
		result = result && (c.getName()==null);
		result = result && (c.getMobile()==null);
		result = result && (c.getEmail1()==null);
		
		return result;
	}

	public static void updateUserDataGroup(String contactBizId, String contactBizDataGroupId)
	throws Exception {
		if (contactBizDataGroupId != null) {
			// ensure contact User has matching datagroup

			Persistence pers = CORE.getPersistence();
			DocumentQuery qUser = pers.newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
			qUser.getFilter().addEquals(Binder.createCompoundBinding(User.contactPropertyName, Bean.DOCUMENT_ID), contactBizId);
			
			List<User> users = qUser.beanResults();
			if (users.size() > 0) {
				User user = users.get(0);
				if (!ModulesUtil.bothNullOrEqual(contactBizDataGroupId, user.getBizDataGroupId())) {

					// set user bizDataGroup and DataGroup reference
					user.setBizDataGroupId(contactBizDataGroupId);

					DocumentQuery qDG = pers.newDocumentQuery(DataGroup.MODULE_NAME, DataGroup.DOCUMENT_NAME);
					qDG.getFilter().addEquals(Bean.DATA_GROUP_ID, contactBizDataGroupId);

					DataGroup dg = qDG.retrieveBean();
					user.setDataGroup(dg);

					// save User
					Customer customer = pers.getUser().getCustomer();
					Module module = customer.getModule(User.MODULE_NAME);
					Document document = module.getDocument(customer, User.DOCUMENT_NAME);

					user = pers.save(document, user);
				}
			}
		}
	}
	
	@Override
	public void preSave(Contact bean) throws Exception {
		if(bean.isChanged() && !bean.isAllowUpdate()){
			throw new ValidationException(new Message("You do not have access to update the contact details for " + bean.getBizKey()));
		}
		updateUserDataGroup(bean.getBizId(), bean.getBizDataGroupId());
		
	}
}
