package modules.whosin;

import modules.admin.domain.Contact;
import modules.whosin.domain.Office;
import modules.whosin.domain.Staff;
import modules.whosin.domain.Staff.Status;

import org.skyve.CORE;
import org.skyve.domain.types.DateOnly;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.wildcat.util.TimeUtil;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

public class WhosInUtil {

	/**
	 * Generates a set of example data for demonstration
	 * 
	 */
	public static void generateExampleData() throws Exception{
		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer= user.getCustomer();
		
		//create example offices
		Module officeModule = customer.getModule(Office.MODULE_NAME);
		Document officeDocument = officeModule.getDocument(customer, Office.DOCUMENT_NAME);
		
		//create the first office
		Office newOffice1 = officeDocument.newInstance(user);
		newOffice1.setLevelUnit("Level 13");
		newOffice1.setStreetAddress("25 Grenfell Street");
		newOffice1.setSuburb("Adelaide");
		newOffice1.setPostCode("5000");

		//create the boundary for office 1
		GeometryFactory gf1 = new GeometryFactory();
		Coordinate[] vertices1 = new Coordinate[4];
		vertices1[0] = new Coordinate(-34.923,138.566);
		vertices1[1] = new Coordinate(-34.924,138.566);
		vertices1[2] = new Coordinate(-34.924,138.567);
		vertices1[3] = new Coordinate(-34.923,138.567);
		vertices1[4] = vertices1[0];
		
		Geometry boundary1 = gf1.createLinearRing(vertices1);
		newOffice1.setBoundary(boundary1);
		newOffice1 = persistence.save(newOffice1);		

		//create Staff
		Module adminModule = customer.getModule(Contact.MODULE_NAME);
		Document contactDocument = adminModule.getDocument(customer, Contact.DOCUMENT_NAME); 
		Document staffDocument = officeModule.getDocument(customer, Staff.DOCUMENT_NAME);
		
		//create Melissa Brown
		Contact contact1 = contactDocument.newInstance(user);
		contact1.setName("Melissa Brown");
		contact1.setMobile("0472 123 123");
		contact1.setEmail1("melissa.brown@whosin.com");
		
		Staff staff1 = staffDocument.newInstance(user);
		staff1.setContact(contact1);
		staff1.setStaffCode("1001");
		
		DateOnly birthday1 = new DateOnly();
		TimeUtil.addMonths(birthday1, -372);
		staff1.setDateOfBirth(birthday1);
		staff1.setRoleTitle("Manager");
		staff1.setBaseOffice(newOffice1);
		staff1.setLocation(boundary1.getCentroid());
		staff1.setStatus(Status.inTheOffice);

		staff1 = persistence.save(staff1);

		//create Mike Smith
		Contact contact2 = contactDocument.newInstance(user);
		contact2.setName("Mike Smith");
		contact2.setMobile("0472 123 123");
		contact2.setEmail1("mike.smith@whosin.com");
		
		Staff staff2 = staffDocument.newInstance(user);
		staff2.setContact(contact2);
		staff2.setStaffCode("1001");
		
		DateOnly birthday2 = new DateOnly();
		TimeUtil.addMonths(birthday2, -384);
		staff2.setDateOfBirth(birthday2);
		staff2.setRoleTitle("Receptionist");
		staff2.setBaseOffice(newOffice1);
		staff2.setLocation(boundary1.getCentroid());
		staff2.setStatus(Status.inTheOffice);

		staff2 = persistence.save(staff2);

		//create Paula Willis
		Contact contact3 = contactDocument.newInstance(user);
		contact3.setName("Paula Willis");
		contact3.setMobile("0472 123 123");
		contact3.setEmail1("paula.willis@whosin.com");
		
		Staff staff3 = staffDocument.newInstance(user);
		staff3.setContact(contact3);
		staff3.setStaffCode("1001");
		
		DateOnly birthday3 = new DateOnly();
		TimeUtil.addMonths(birthday3, -384);
		staff3.setDateOfBirth(birthday3);
		staff3.setRoleTitle("Accountant");
		staff3.setBaseOffice(newOffice1);
		staff3.setLocation(boundary1.getCentroid());
		staff3.setStatus(Status.inTheOffice);

		staff3 = persistence.save(staff3);

		
		//create the second office
		Office newOffice2 = officeDocument.newInstance(user);
		newOffice2.setStreetAddress("1 Northcote Street");
		newOffice2.setSuburb("Torrensville");
		newOffice2.setPostCode("5031");
				
		//create the boundary for office 2
		GeometryFactory gf2 = new GeometryFactory();
		Coordinate[] vertices2  = new Coordinate[4];
		vertices2[0] = new Coordinate(-34.923,138.600);
		vertices2[1] = new Coordinate(-34.924,138.600);
		vertices2[2] = new Coordinate(-34.924,138.601);
		vertices2[3] = new Coordinate(-34.923,138.601);
		vertices2[4] = vertices2[0];
		
		Geometry boundary2 = gf2.createLinearRing(vertices2);
		newOffice2.setBoundary(boundary2);		
		newOffice2 = persistence.save(newOffice2);

		//create Frank Jones
		Contact contact4 = contactDocument.newInstance(user);
		contact4.setName("Frank Jones");
		contact4.setMobile("0472 123 123");
		contact4.setEmail1("frank.jones@whosin.com");
		
		Staff staff4 = staffDocument.newInstance(user);
		staff4.setContact(contact4);
		staff4.setStaffCode("1001");
		
		DateOnly birthday4 = new DateOnly();
		TimeUtil.addMonths(birthday4, -372);
		staff4.setDateOfBirth(birthday4);
		staff4.setRoleTitle("Accountant");
		staff4.setBaseOffice(newOffice2);
		staff4.setLocation(boundary2.getCentroid());
		staff4.setStatus(Status.inTheOffice);

		staff4 = persistence.save(staff4);

		//create Michelle Skyve
		Contact contact5 = contactDocument.newInstance(user);
		contact5.setName("Michelle Skyve");
		contact5.setMobile("0472 123 123");
		contact5.setEmail1("michelle.skyve@whosin.com");
		
		Staff staff5 = staffDocument.newInstance(user);
		staff5.setContact(contact5);
		staff5.setStaffCode("1001");
		
		DateOnly birthday5 = new DateOnly();
		TimeUtil.addMonths(birthday5, -384);
		staff5.setDateOfBirth(birthday5);
		staff5.setRoleTitle("Accountant");
		staff5.setBaseOffice(newOffice1);
		staff5.setLocation(boundary1.getCentroid());
		staff5.setStatus(Status.inTheOffice);

		staff5 = persistence.save(staff5);

	}
}
