package modules.whosin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import java.time.LocalDate;
import java.time.Month;

import org.skyve.domain.types.DateOnly;

@SuppressWarnings("static-method")
class StaffQualificationDomainTest {

	private StaffQualification bean;

	@BeforeEach
	void setUp() {
		bean = new StaffQualification();
	}

	@Test
	void bizModuleIsWhosin() {
		assertEquals("whosin", bean.getBizModule());
	}

	@Test
	void bizDocumentIsStaffQualification() {
		assertEquals("StaffQualification", bean.getBizDocument());
	}

	@Test
	void moduleNameConstant() {
		assertEquals("whosin", StaffQualification.MODULE_NAME);
	}

	@Test
	void documentNameConstant() {
		assertEquals("StaffQualification", StaffQualification.DOCUMENT_NAME);
	}

	@Test
	void propertyNameConstants() {
		assertEquals("type", StaffQualification.typePropertyName);
		assertEquals("name", StaffQualification.namePropertyName);
		assertEquals("issuingOrganisation", StaffQualification.issuingOrganisationPropertyName);
		assertEquals("description", StaffQualification.descriptionPropertyName);
		assertEquals("dateAttained", StaffQualification.dateAttainedPropertyName);
		assertEquals("dateExpiry", StaffQualification.dateExpiryPropertyName);
	}

	@Test
	void typeSetAndGet() {
		bean.setType(StaffQualification.Type.skill);
		assertEquals(StaffQualification.Type.skill, bean.getType());
		bean.setType(StaffQualification.Type.master);
		assertEquals(StaffQualification.Type.master, bean.getType());
		bean.setType(StaffQualification.Type.phD);
		assertEquals(StaffQualification.Type.phD, bean.getType());
	}

	@Test
	void nameSetAndGet() {
		bean.setName("Java SE 17");
		assertEquals("Java SE 17", bean.getName());
	}

	@Test
	void issuingOrganisationSetAndGet() {
		bean.setIssuingOrganisation("Oracle");
		assertEquals("Oracle", bean.getIssuingOrganisation());
	}

	@Test
	void descriptionSetAndGet() {
		bean.setDescription("Core Java certification");
		assertEquals("Core Java certification", bean.getDescription());
	}

	@Test
	void dateAttainedSetAndGet() {
		DateOnly date = new DateOnly(LocalDate.of(2023, Month.MAY, 10));
		bean.setDateAttained(date);
		assertEquals(date, bean.getDateAttained());
	}

	@Test
	void dateExpirySetAndGet() {
		DateOnly date = new DateOnly(LocalDate.of(2026, Month.MAY, 10));
		bean.setDateExpiry(date);
		assertEquals(date, bean.getDateExpiry());
	}

	@Test
	void parentSetAndGet() {
		Staff parent = new Staff();
		bean.setParent(parent);
		assertEquals(parent, bean.getParent());
	}

	@Test
	void parentInitiallyNull() {
		assertNull(bean.getParent());
	}

	@Test
	void typeEnumFromCode() {
		assertEquals(StaffQualification.Type.skill, StaffQualification.Type.fromCode("Skill"));
		assertEquals(StaffQualification.Type.phD, StaffQualification.Type.fromCode("PhD"));
		assertNull(StaffQualification.Type.fromCode("nonexistent"));
	}

	@Test
	void typeEnumToDomainValues() {
		assertNotNull(StaffQualification.Type.toDomainValues());
		assertFalse(StaffQualification.Type.toDomainValues().isEmpty());
	}

	@Test
	void typeEnumToCode() {
		assertEquals("Skill", StaffQualification.Type.skill.toCode());
	}

	@Test
	void typeEnumToLocalisedDescription() {
		assertNotNull(StaffQualification.Type.skill.toLocalisedDescription());
	}

	@Test
	void typeEnumToDomainValue() {
		assertNotNull(StaffQualification.Type.skill.toDomainValue());
	}

	@Test
	void typeEnumFromLocalisedDescription() {
		String description = StaffQualification.Type.skill.toLocalisedDescription();
		assertEquals(StaffQualification.Type.skill, StaffQualification.Type.fromLocalisedDescription(description));
	}

	@Test
	void typeEnumFromLocalisedDescriptionUnknownReturnsNull() {
		assertNull(StaffQualification.Type.fromLocalisedDescription("nonexistent description xyz"));
	}

	@Test
	void getBizKeyWithNameAndTypeSet() {
		bean.setName("Java SE");
		bean.setType(StaffQualification.Type.skill);
		assertNotNull(bean.getBizKey());
	}

	@Test
	void bizOrdinalSetAndGet() {
		bean.setBizOrdinal(Integer.valueOf(3));
		assertEquals(Integer.valueOf(3), bean.getBizOrdinal());
	}
}
