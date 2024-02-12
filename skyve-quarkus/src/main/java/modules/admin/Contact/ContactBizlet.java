package modules.admin.Contact;

import org.skyve.metadata.model.document.Bizlet;

import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;

public class ContactBizlet extends Bizlet<Contact> {
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

}
