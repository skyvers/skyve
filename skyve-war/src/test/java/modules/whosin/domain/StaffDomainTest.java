package modules.whosin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import java.time.LocalDate;

import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;

@SuppressWarnings("static-method")
class StaffDomainTest {

	private Staff bean;

	@BeforeEach
	void setUp() {
		bean = new Staff();
	}

	@Test
	void bizModuleIsWhosin() {
		assertEquals("whosin", bean.getBizModule());
	}

	@Test
	void bizDocumentIsStaff() {
		assertEquals("Staff", bean.getBizDocument());
	}

	@Test
	void moduleNameConstant() {
		assertEquals("whosin", Staff.MODULE_NAME);
	}

	@Test
	void documentNameConstant() {
		assertEquals("Staff", Staff.DOCUMENT_NAME);
	}

	@Test
	void propertyNameConstants() {
		assertEquals("contact", Staff.contactPropertyName);
		assertEquals("staffCode", Staff.staffCodePropertyName);
		assertEquals("dateOfBirth", Staff.dateOfBirthPropertyName);
		assertEquals("roleTitle", Staff.roleTitlePropertyName);
		assertEquals("baseOffice", Staff.baseOfficePropertyName);
		assertEquals("status", Staff.statusPropertyName);
		assertEquals("dueBack", Staff.dueBackPropertyName);
		assertEquals("demoData", Staff.demoDataPropertyName);
		assertEquals("qualifications", Staff.qualificationsPropertyName);
	}

	@Test
	void staffCodeSetAndGet() {
		bean.setStaffCode("SC001");
		assertEquals("SC001", bean.getStaffCode());
	}

	@Test
	void dateOfBirthSetAndGet() {
		DateOnly dob = new DateOnly(LocalDate.of(2000, 1, 15));
		bean.setDateOfBirth(dob);
		assertEquals(dob, bean.getDateOfBirth());
	}

	@Test
	void roleTitleSetAndGet() {
		bean.setRoleTitle("Senior Developer");
		assertEquals("Senior Developer", bean.getRoleTitle());
	}

	@Test
	void statusSetAndGet() {
		bean.setStatus(Staff.Status.inTheOffice);
		assertEquals(Staff.Status.inTheOffice, bean.getStatus());
		bean.setStatus(Staff.Status.outOfTheOffice);
		assertEquals(Staff.Status.outOfTheOffice, bean.getStatus());
		bean.setStatus(Staff.Status.onLeave);
		assertEquals(Staff.Status.onLeave, bean.getStatus());
		bean.setStatus(Staff.Status.atLunch);
		assertEquals(Staff.Status.atLunch, bean.getStatus());
	}

	@Test
	void dueBackSetAndGet() {
		DateTime dt = new DateTime();
		bean.setDueBack(dt);
		assertEquals(dt, bean.getDueBack());
	}

	@Test
	void demoDataSetAndGet() {
		bean.setDemoData(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getDemoData());
		bean.setDemoData(Boolean.FALSE);
		assertEquals(Boolean.FALSE, bean.getDemoData());
	}

	@Test
	void reportsToSetAndGet() {
		Staff manager = new Staff();
		bean.setReportsTo(manager);
		assertEquals(manager, bean.getReportsTo());
	}

	@Test
	void qualificationsListInitiallyEmpty() {
		assertNotNull(bean.getQualifications());
		assertTrue(bean.getQualifications().isEmpty());
	}

	@Test
	void addQualificationsElementSetsParent() {
		StaffQualification qual = new StaffQualification();
		boolean added = bean.addQualificationsElement(qual);
		assertTrue(added);
		assertEquals(bean, qual.getParent());
		assertEquals(1, bean.getQualifications().size());
	}

	@Test
	void removeQualificationsElementClearsParent() {
		StaffQualification qual = new StaffQualification();
		bean.addQualificationsElement(qual);
		boolean removed = bean.removeQualificationsElement(qual);
		assertTrue(removed);
		assertNull(qual.getParent());
		assertTrue(bean.getQualifications().isEmpty());
	}

	@Test
	void removeQualificationsElementNotPresent() {
		StaffQualification qual = new StaffQualification();
		assertFalse(bean.removeQualificationsElement(qual));
	}

	@Test
	void bizParentIdSetAndGet() {
		bean.setBizParentId("parent-123");
		assertEquals("parent-123", bean.getBizParentId());
	}

	@Test
	void statusEnumFromCode() {
		assertEquals(Staff.Status.inTheOffice, Staff.Status.fromCode("inOffice"));
		assertEquals(Staff.Status.outOfTheOffice, Staff.Status.fromCode("outOffice"));
		assertEquals(Staff.Status.onLeave, Staff.Status.fromCode("onLeave"));
		assertEquals(Staff.Status.atLunch, Staff.Status.fromCode("atLunch"));
		assertNull(Staff.Status.fromCode("unknown"));
	}

	@Test
	void statusEnumToDomainValues() {
		assertNotNull(Staff.Status.toDomainValues());
		assertFalse(Staff.Status.toDomainValues().isEmpty());
	}

	@Test
	void statusEnumToCode() {
		assertEquals("inOffice", Staff.Status.inTheOffice.toCode());
	}

	@Test
	void statusEnumToLocalisedDescription() {
		assertNotNull(Staff.Status.inTheOffice.toLocalisedDescription());
	}

	@Test
	void statusEnumToDomainValue() {
		assertNotNull(Staff.Status.inTheOffice.toDomainValue());
	}

	@Test
	void statusEnumFromLocalisedDescription() {
		String desc = Staff.Status.inTheOffice.toLocalisedDescription();
		assertEquals(Staff.Status.inTheOffice, Staff.Status.fromLocalisedDescription(desc));
	}

	@Test
	void statusEnumFromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(Staff.Status.fromLocalisedDescription("nonexistent xyz description"));
	}

	@Test
	void contactSetAndGet() {
		modules.admin.Contact.ContactExtension contact = new modules.admin.Contact.ContactExtension();
		bean.setContact(contact);
		assertEquals(contact, bean.getContact());
	}

	@Test
	void baseOfficeSetAndGet() {
		Office office = new Office();
		bean.setBaseOffice(office);
		assertEquals(office, bean.getBaseOffice());
	}

	@Test
	void addQualificationsElementAtIndex() {
		StaffQualification qual = new StaffQualification();
		bean.addQualificationsElement(0, qual);
		assertEquals(1, bean.getQualifications().size());
	}

	@Test
	void removeQualificationsElementByIndex() {
		StaffQualification qual = new StaffQualification();
		bean.addQualificationsElement(qual);
		StaffQualification removed = bean.removeQualificationsElement(0);
		assertEquals(qual, removed);
		assertTrue(bean.getQualifications().isEmpty());
	}

	@Test
	void getQualificationsElementByIdReturnsNullForUnknownId() {
		// No elements added, so any bizId lookup returns null
		assertNull(bean.getQualificationsElementById("unknown-id"));
	}
}
