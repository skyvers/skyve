package modules.whosin;

import java.util.Date;
import java.util.List;
import java.util.Random;

import modules.admin.domain.Contact;
import modules.admin.domain.Contact.ContactType;
import modules.whosin.domain.Office;
import modules.whosin.domain.Staff;
import modules.whosin.domain.Staff.Status;

import org.skyve.CORE;
import org.skyve.domain.types.DateOnly;
import org.skyve.job.WildcatJob;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.wildcat.util.TimeUtil;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;

/**
 * Load Demonstration Data for the Who's In? module
 * 
 * @author rob
 */
public class LoadDemonstrationDataJob extends WildcatJob {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 8903261480076338400L;

	@Override
	public String cancel() {
		return null;
	}

	@Override
	public void execute() throws Exception {
		List<String> log = getLog();
		try {
			log.add("Started Loading Demonstration Data at " + new Date());

			Persistence persistence = CORE.getPersistence();
			clearPreviousData(persistence);
			
			//In this case, it's so quick, it's one step
			generateExampleData(persistence);

			setPercentComplete(100);
			log.add("Finished Loading Demonstration Data at " + new Date());
		} catch (Throwable t) {
			log.add("Encountered an error during the job run: " + t.getMessage());
			t.printStackTrace();
		}
	}

	/**
	 * Generates a set of example data for demonstration
	 * 
	 */
	public static void generateExampleData(Persistence persistence) throws Exception {

		User user = persistence.getUser();
		Customer customer = user.getCustomer();

		// create example offices
		Module officeModule = customer.getModule(Office.MODULE_NAME);
		Document officeDocument = officeModule.getDocument(customer, Office.DOCUMENT_NAME);

		// create the first office
		Office newOffice1 = officeDocument.newInstance(user);
		newOffice1.setLevelUnit("Level 13");
		newOffice1.setStreetAddress("25 Grenfell Street");
		newOffice1.setSuburb("Adelaide");
		newOffice1.setPostCode("5000");

		// create the boundary for office 1
		GeometryFactory gf1 = new GeometryFactory();
		Coordinate[] vertices1 = new Coordinate[5];
		vertices1[0] = new Coordinate(138.60065907239914, -34.92455289170784);
		vertices1[1] = new Coordinate(138.6006858944893, -34.92489156179162);
		vertices1[2] = new Coordinate(138.6010828614235, -34.924878366879454);
		vertices1[3] = new Coordinate(138.6010479927063, -34.92453090009563);
		vertices1[4] = vertices1[0];

		Geometry boundary1 = gf1.createLinearRing(vertices1);
		newOffice1.setBoundary(boundary1);
		newOffice1.setDemoData(Boolean.TRUE); //remember that we created this
		
		newOffice1 = persistence.save(newOffice1);

		for(int i=0;i<80;i++){
			createRandomStaff(persistence, officeModule, customer, user, newOffice1, boundary1.getCentroid(), i);
		}

		//create staff in random states and positions
		for(int i=0;i<40;i++){
			createRandomStaff(persistence, officeModule, customer, user, newOffice1, null, i);
		}

		// create the second office
		Office newOffice2 = officeDocument.newInstance(user);
		newOffice2.setStreetAddress("1 Northcote Street");
		newOffice2.setSuburb("Torrensville");
		newOffice2.setPostCode("5031");

		// create the boundary for office 2
		GeometryFactory gf2 = new GeometryFactory();
		Coordinate[] vertices2 = new Coordinate[6];

		vertices2[0] = new Coordinate(138.56729239225388, -34.92350608442984);
		vertices2[1] = new Coordinate(138.5672977566719, -34.923693015280314);
		vertices2[2] = new Coordinate(138.56737285852432, -34.923739198130725);
		vertices2[3] = new Coordinate(138.5674586892128, -34.923739198130725);
		vertices2[4] = new Coordinate(138.56744527816772, -34.92352587712828);
		vertices2[5] = vertices2[0];

		Geometry boundary2 = gf2.createLinearRing(vertices2);
		newOffice2.setBoundary(boundary2);
		newOffice2.setDemoData(Boolean.TRUE); //remember that we created this
		
		newOffice2 = persistence.save(newOffice2);

		for(int i=0;i<40;i++){
			createRandomStaff(persistence, officeModule, customer, user, newOffice2, boundary2.getCentroid(), i);
		}

		//create staff in random states and positions
		for(int i=0;i<40;i++){
			createRandomStaff(persistence, officeModule, customer, user, newOffice2, null, i);
		}
		
	}

	public static void createRandomStaff(Persistence persistence, Module module, Customer customer, User user, Office baseOffice, Geometry location, int index)
	throws Exception{
		
		//random values
		String[] firstNames = { "Valentina", "Sofia", "Desiree", "Malia", "Nina", "Alexandra", "Brooke", "Jayla", "Raegan", "Valerie", "Allie", "Stella", "Molly", "Lauren", "Nevaeh", "Jillian", "Kaydence", "Alejandra", "Autumn", "Zoe", "Jordyn", "Alexandra", "Jazlyn", "Layla", "Madeline", "Gabriela", "Kayden", "Scarlett", "Riley", "Itzel", "Katelyn", "Addison", "Fiona", "Jordan", "Lyla", "Sarah", "Natalia", "Aaliyah", "Sabrina", "Alexandria", "Jimena", "Delaney", "Katelynn", "Kathryn",
				"Briana", "Aaliyah", "Carly", "Allison", "Shelby", "Avery" };
		String[] lastNames = { "Talbert", "Lundy", "Harlow", "Courtney", "Woodson", "Burroughs", "Little", "Irving", "Stanford", "Chang", "David", "Rowe", "Munson", "Kinney", "Bonilla", "Tomlin", "Spicer", "Ackerman", "Childress", "Caruso", "Gillespie", "Borden", "Brewer", "Lay", "Dowdy", "Woodard", "Casey", "Latham", "Rosenthal", "Collazo", "Bonds", "Crow", "Blum", "Arnold", "Mckay", "Pereira", "Huynh", "Gooden", "Windham", "Kenny", "Pappas", "Steward", "Skinner", "James", "Bowling", "Decker",
				"Lane", "Griffith", "Cline", "Gresham" };
		String[] roles = { "Manager", "Sales Manager", "Sales Consultant", "Sales Support Technician", "Accountant", "Receptionist"};
		String[] digits = {"0","1","2","3","4","5","6","7","8","9"};
		Status[] inOfficeStates = {Status.inTheOffice, Status.atLunch};
		Status[] outOfOfficeStates = {Status.outOfTheOffice, Status.atLunch, Status.onLeave};

		//generate random indices
		int firstNameIndex = new Random().nextInt(firstNames.length);
		int lastNameIndex = new Random().nextInt(lastNames.length);
		int roleIndex = new Random().nextInt(roles.length);

		//create values
		StringBuilder fullName =  new StringBuilder();
		fullName.append(firstNames[firstNameIndex]).append(' ').append(lastNames[lastNameIndex]);
		
		int daysOld = 7300 + new Random().nextInt(15000); //age range, 20 to 60ish
		DateOnly birthday = new DateOnly();
		TimeUtil.addDays(birthday, -1 * daysOld);

		StringBuilder mobile = new StringBuilder();
		mobile.append("04");
		for(int i =0; i<2;i++){
			mobile.append(digits[new Random().nextInt(digits.length)]);
		}
		for(int k = 0;k<2;k++){
			mobile.append(' ');
			for(int i =0; i<3;i++){
				mobile.append(digits[new Random().nextInt(digits.length)]);
			}			
		}
		
		// create Staff
		Module adminModule = customer.getModule(Contact.MODULE_NAME);
		Document contactDocument = adminModule.getDocument(customer, Contact.DOCUMENT_NAME);
		Document staffDocument = module.getDocument(customer, Staff.DOCUMENT_NAME);

		Contact contact = contactDocument.newInstance(user);
		contact.setName(fullName.toString());
		contact.setMobile(mobile.toString());
		contact.setEmail1(fullName.toString().toLowerCase().replace(' ', '.') +  "@whosin.com");
		contact.setContactType(ContactType.person);

		Staff staff = staffDocument.newInstance(user);
		staff.setContact(contact);
		staff.setStaffCode(Integer.toString(index));

		staff.setDateOfBirth(birthday);
		staff.setRoleTitle(roles[roleIndex]);
		staff.setBaseOffice(baseOffice);
		
		//set location 
		if(location!=null){
			staff.setLocation(location);
			
		} else {
			//generate a random location in the vicinity of the base office
			double x = baseOffice.getBoundary().getCentroid().getX();
			double y = baseOffice.getBoundary().getCentroid().getY();
			
			//generate only positive offsets as anything to the west would be in the ocean!
			double offsetX = (double) new Random().nextInt(10000)-5000;
			double offsetY = (double) new Random().nextInt(10000)-5000;
			x += offsetX/100000;
			y += offsetY/100000;
			
			GeometryFactory gf = new GeometryFactory();
			Coordinate c = new Coordinate(x, y);
			Geometry point = gf.createPoint(c);
			staff.setLocation(point);
		}
		
		//set status corresponding to position
		if(baseOffice.getBoundary().contains(staff.getLocation())){
			staff.setStatus(inOfficeStates[new Random().nextInt(inOfficeStates.length)]);	
		} else {
			staff.setStatus(outOfOfficeStates[new Random().nextInt(outOfOfficeStates.length)]);	
		}

		staff.setDemoData(Boolean.TRUE); //remember that we created this
		
		staff = persistence.save(staff);
		
	}
	
	public static void clearPreviousData(Persistence pers) throws Exception {

		//try to delete all staff (some may fail if users have been created)
		DocumentQuery qStaff = pers.newDocumentQuery(Staff.MODULE_NAME, Staff.DOCUMENT_NAME);
		qStaff.getFilter().addEquals(Staff.demoDataPropertyName, Boolean.TRUE);
		
		List<Staff> allStaff = pers.retrieve(qStaff);
		for(Staff s: allStaff){
			try{
				Contact c = s.getContact();
				pers.delete(s);
				pers.delete(c);
				pers.commit(false);
				pers.evictCached(s);
				pers.evictCached(c);
				
				//after evicting start a new transaction
				pers.begin(); 
			} catch (Exception e){
				//I figure these are probably not intended to be deleted, so just fail and move on				
			}
		}
		
		//try to delete all offices (some may fail if users have been created)
		DocumentQuery qOffice = pers.newDocumentQuery(Office.MODULE_NAME, Office.DOCUMENT_NAME);
		qOffice.getFilter().addEquals(Office.demoDataPropertyName, Boolean.TRUE);
		
		List<Office> allOffices = pers.retrieve(qOffice);
		for(Office o: allOffices){
			try{
				pers.delete(o);
				pers.commit(false);
				pers.evictCached(o);
				pers.begin();
			} catch (Exception e){
				//I figure these are probably not intended to be deleted, so just fail and move on				
			}
		}
		
		
	}
}
