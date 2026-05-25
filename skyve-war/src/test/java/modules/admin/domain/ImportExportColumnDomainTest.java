package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.ImportExportColumn.LoadAction;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class ImportExportColumnDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderCreatesBean() throws Exception {
		ImportExportColumn bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(ImportExportColumn.MODULE_NAME, ImportExportColumn.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	void moduleAndDocumentNames() throws Exception {
		ImportExportColumn bean = ImportExportColumn.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("ImportExportColumn", bean.getBizDocument());
	}

	@Test
	void columnNameSetAndGet() throws Exception {
		ImportExportColumn bean = ImportExportColumn.newInstance();
		bean.setColumnName("First Name");
		assertEquals("First Name", bean.getColumnName());
	}

	@Test
	void bindingNameSetAndGet() throws Exception {
		ImportExportColumn bean = ImportExportColumn.newInstance();
		bean.setBindingName("contact.name");
		assertEquals("contact.name", bean.getBindingName());
	}

	@Test
	void bindingExpressionSetAndGet() throws Exception {
		ImportExportColumn bean = ImportExportColumn.newInstance();
		bean.setBindingExpression("{contact.name} {contact.lastName}");
		assertEquals("{contact.name} {contact.lastName}", bean.getBindingExpression());
	}

	@Test
	void loadActionSetAndGet() throws Exception {
		ImportExportColumn bean = ImportExportColumn.newInstance();
		bean.setLoadAction(LoadAction.setValue);
		assertEquals(LoadAction.setValue, bean.getLoadAction());
	}

	@Test
	void isNotShowExpressionWhenBindingNameIsNull() throws Exception {
		ImportExportColumn bean = ImportExportColumn.newInstance();
		assertNull(bean.getBindingName());
		assertFalse(bean.isShowExpression());
		assertTrue(bean.isNotShowExpression());
	}

	@Test
	void isShowExpressionWhenBindingNameIsExpression() throws Exception {
		ImportExportColumn bean = ImportExportColumn.newInstance();
		bean.setBindingName(modules.admin.ImportExport.ImportExportUtil.EXPRESSION);
		assertTrue(bean.isShowExpression());
		assertFalse(bean.isNotShowExpression());
	}

	@Test
	void loadActionFromCode() throws Exception {
		assertEquals(LoadAction.setValue, LoadAction.fromCode("set"));
		assertEquals(LoadAction.lookupEquals, LoadAction.fromCode("equals"));
		assertEquals(LoadAction.lookupLike, LoadAction.fromCode("like"));
		assertEquals(LoadAction.lookupContains, LoadAction.fromCode("contains"));
		assertEquals(LoadAction.confirmValue, LoadAction.fromCode("confirm"));
		assertNull(LoadAction.fromCode("nonexistent"));
	}

	@Test
	void loadActionToCode() throws Exception {
		assertEquals("set", LoadAction.setValue.toCode());
		assertEquals("equals", LoadAction.lookupEquals.toCode());
	}

	@Test
	void loadActionToDomainValues() throws Exception {
		assertNotNull(LoadAction.toDomainValues());
		assertFalse(LoadAction.toDomainValues().isEmpty());
	}

	@Test
	void loadActionToLocalisedDescription() throws Exception {
		assertNotNull(LoadAction.setValue.toLocalisedDescription());
	}

	@Test
	void loadActionToDomainValue() throws Exception {
		assertNotNull(LoadAction.setValue.toDomainValue());
	}

	@Test
	void loadActionFromLocalisedDescription() throws Exception {
		String desc = LoadAction.setValue.toLocalisedDescription();
		assertEquals(LoadAction.setValue, LoadAction.fromLocalisedDescription(desc));
	}

	@Test
	void loadActionFromLocalisedDescriptionUnknownReturnsNull() throws Exception {
		assertNull(LoadAction.fromLocalisedDescription("nonexistent description xyz"));
	}

        @Test
        void getBizKeyNotNull() throws Exception {
                ImportExportColumn bean = ImportExportColumn.newInstance();
                assertNotNull(bean.getBizKey());
        }

        @Test
        void parentNullByDefault() throws Exception {
                ImportExportColumn bean = ImportExportColumn.newInstance();
                assertNull(bean.getParent());
        }

        @Test
        void parentSetAndGet() throws Exception {
                ImportExportColumn bean = ImportExportColumn.newInstance();
                modules.admin.ImportExport.ImportExportExtension parent = new modules.admin.ImportExport.ImportExportExtension();
                bean.setParent(parent);
                assertEquals(parent, bean.getParent());
        }

        @Test
        void bizOrdinalSetAndGet() throws Exception {
                ImportExportColumn bean = ImportExportColumn.newInstance();
                bean.setBizOrdinal(5);
                assertEquals(Integer.valueOf(5), bean.getBizOrdinal());
        }
}
