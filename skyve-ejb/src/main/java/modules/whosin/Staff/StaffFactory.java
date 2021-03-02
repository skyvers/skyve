package modules.whosin.Staff;

import java.util.Random;

import org.skyve.CORE;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.whosin.domain.Office;
import modules.whosin.domain.Staff;

public class StaffFactory {

	@SkyveFixture(types = FixtureType.seed)
	public static Staff seedInstance() {
		
		Staff bean = new DataBuilder().build(Staff.MODULE_NAME, Staff.DOCUMENT_NAME);
		
		//create a new person type contact
		Contact contact = new DataBuilder().build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
		contact.setContactType(ContactType.person);
		bean.setContact(contact);

		//throw the dice to see whether to create a new office, or re-use an existing one
		int dice = new Random().nextInt(50);
		if(dice<49) {
			
			//re-use a random existing office
			DocumentQuery qOffice = CORE.getPersistence().newDocumentQuery(Office.MODULE_NAME, Office.DOCUMENT_NAME);
			
			int officeCount = qOffice.beanResults().size();
			if(officeCount>0) {
				int randomOfficeIndex = new Random().nextInt(officeCount);
				qOffice.setFirstResult(randomOfficeIndex);
				
				Office office = qOffice.beanResult();
				if(office!=null) {
					bean.setBaseOffice(office);
				}
			}
			
		} 
		if(bean.getBaseOffice()==null) {
			// create a new office
			Office office = new DataBuilder().build(Office.MODULE_NAME, Office.DOCUMENT_NAME);
			bean.setBaseOffice(office);
		}

		return bean;
	}

}
