package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Objects;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.controller.CustomisationsStaticSingleton;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Colour;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.FormatterName;
import org.skyve.metadata.controller.Customisations;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;

@SuppressWarnings({"static-method", "boxing"})
class SmartClientQueryColumnDefinitionTest {

	@Test
	void projectedColumnConstructorHonoursProjectedFlags() {
		MetaDataQueryProjectedColumn column = mock(MetaDataQueryProjectedColumn.class);
		when(column.getBinding()).thenReturn(null);
		when(column.getName()).thenReturn("status");
		when(column.getLocalisedDisplayName()).thenReturn("Status");
		when(column.getAlignment()).thenReturn(null);
		when(column.getPixelWidth()).thenReturn(Integer.valueOf(120));
		when(column.isEscape()).thenReturn(true);
		when(column.isHidden()).thenReturn(true);
		when(column.getFormatterName()).thenReturn(FormatterName.Integer);
		when(column.getCustomFormatterName()).thenReturn(null);
		when(column.isFilterable()).thenReturn(false);
		when(column.isSortable()).thenReturn(false);
		when(column.isEditable()).thenReturn(false);

		SmartClientQueryColumnDefinition definition = new SmartClientQueryColumnDefinition(null,
				null,
				null,
				null,
				column,
				false,
				null);

		assertTrue(definition.isDetail());
		assertTrue(definition.isCanSortClientOnly());
		assertTrue(! definition.isCanFilter());
		assertTrue(! definition.isCanSave());

		String javascript = definition.toJavascript();
		assertTrue(javascript.contains("name:'status'"));
		assertTrue(javascript.contains("title:'Status'"));
		assertTrue(javascript.contains("canFilter:false"));
		assertTrue(javascript.contains("canSave:false"));
		assertTrue(javascript.contains("detail:true"));
		assertTrue(javascript.contains("canSortClientOnly:true"));
		assertTrue(javascript.contains("sortByField:'status'"));
		assertTrue(javascript.contains("escapeHTML:true"));
		assertTrue(javascript.contains("width:120"));
	}

	@Test
	void contentThumbnailColumnSetsImageTypeAndDefaultHeight() {
		MetaDataQueryContentColumn column = mock(MetaDataQueryContentColumn.class);
		when(column.getBinding()).thenReturn(null);
		when(column.getName()).thenReturn("photo");
		when(column.getLocalisedDisplayName()).thenReturn(null);
		when(column.getAlignment()).thenReturn(null);
		when(column.getPixelWidth()).thenReturn(Integer.valueOf(80));
		when(column.isEscape()).thenReturn(true);
		when(column.isHidden()).thenReturn(false);
		when(column.getPixelHeight()).thenReturn(null);
		when(column.getEmptyThumbnailRelativeFile()).thenReturn("images/empty.png");
		when(column.getDisplay()).thenReturn(DisplayType.thumbnail);

		SmartClientQueryColumnDefinition definition = new SmartClientQueryColumnDefinition(null,
				null,
				null,
				null,
				column,
				false,
				null);

		assertEquals(Integer.valueOf(64), definition.getPixelHeight());

		String javascript = definition.toJavascript();
		assertTrue(javascript.contains("type:'image'"));
		assertTrue(javascript.contains("canFilter:false"));
		assertTrue(javascript.contains("canSave:false"));
		assertTrue(javascript.contains("width:88"));
		assertTrue(javascript.contains("images/empty.png"));
	}

	@Test
	void contentLinkColumnSetsLinkType() {
		MetaDataQueryContentColumn column = mock(MetaDataQueryContentColumn.class);
		when(column.getBinding()).thenReturn(null);
		when(column.getName()).thenReturn("attachment");
		when(column.getLocalisedDisplayName()).thenReturn("Attachment");
		when(column.getAlignment()).thenReturn(null);
		when(column.getPixelWidth()).thenReturn(Integer.valueOf(100));
		when(column.isEscape()).thenReturn(true);
		when(column.isHidden()).thenReturn(false);
		when(column.getPixelHeight()).thenReturn(Integer.valueOf(72));
		when(column.getEmptyThumbnailRelativeFile()).thenReturn(null);
		when(column.getDisplay()).thenReturn(DisplayType.link);

		SmartClientQueryColumnDefinition definition = new SmartClientQueryColumnDefinition(null,
				null,
				null,
				null,
				column,
				false,
				null);

		String javascript = definition.toJavascript();
		assertTrue(javascript.contains("type:'link'"));
		assertTrue(javascript.contains("canFilter:false"));
		assertTrue(javascript.contains("canSave:false"));
	}

	@Test
	void contentThumbnailColumnPreservesExplicitHeight() {
		MetaDataQueryContentColumn column = mock(MetaDataQueryContentColumn.class);
		when(column.getBinding()).thenReturn(null);
		when(column.getName()).thenReturn("photo");
		when(column.getLocalisedDisplayName()).thenReturn("Photo");
		when(column.getAlignment()).thenReturn(null);
		when(column.getPixelWidth()).thenReturn(Integer.valueOf(80));
		when(column.isEscape()).thenReturn(true);
		when(column.isHidden()).thenReturn(false);
		when(column.getPixelHeight()).thenReturn(Integer.valueOf(96));
		when(column.getEmptyThumbnailRelativeFile()).thenReturn(null);
		when(column.getDisplay()).thenReturn(DisplayType.thumbnail);

		SmartClientQueryColumnDefinition definition = new SmartClientQueryColumnDefinition(null,
				null,
				null,
				null,
				column,
				false,
				null);

		assertEquals(Integer.valueOf(96), definition.getPixelHeight());
		assertTrue(definition.toJavascript().contains("type:'image'"));
	}

	@Test
	void projectedColumnWithoutFormatterKeepsDefaultDisplayFieldOff() {
		MetaDataQueryProjectedColumn column = mock(MetaDataQueryProjectedColumn.class);
		when(column.getBinding()).thenReturn(null);
		when(column.getName()).thenReturn("status");
		when(column.getLocalisedDisplayName()).thenReturn("Status");
		when(column.getAlignment()).thenReturn(null);
		when(column.getPixelWidth()).thenReturn(null);
		when(column.isEscape()).thenReturn(true);
		when(column.isHidden()).thenReturn(false);
		when(column.getFormatterName()).thenReturn(null);
		when(column.getCustomFormatterName()).thenReturn(null);
		when(column.isFilterable()).thenReturn(true);
		when(column.isSortable()).thenReturn(true);
		when(column.isEditable()).thenReturn(true);

		SmartClientQueryColumnDefinition definition = new SmartClientQueryColumnDefinition(null,
				null,
				null,
				null,
				column,
				false,
				null);

		assertFalse(definition.isHasDisplayField());
		assertTrue(! definition.toJavascript().contains("sortByField:'status'"));
	}

	@Test
	void projectedColumnWithConstantDomainUsesEqualsOperators() throws Exception {
		Text status = new Text();
		status.setName("status");
		status.setDisplayName("Status");
		status.setDomainType(DomainType.constant);

		SmartClientQueryColumnDefinition definition = definitionFor(status);

		String javascript = definition.toJavascript();
		assertTrue(javascript.contains("type:'enum'"));
		assertTrue(javascript.contains("validOperators:['equals','notEqual','isNull','notNull']"));
	}

	@Test
	void projectedColumnWithVariantDomainUsesTextFilteringAndContainsOperators() throws Exception {
		Text status = new Text();
		status.setName("status");
		status.setDisplayName("Status");
		status.setDomainType(DomainType.variant);

		SmartClientQueryColumnDefinition definition = definitionFor(status);

		assertTrue(definition.getHasTextFilterOperators());
		assertFalse(definition.isCanSave());
		String javascript = definition.toJavascript();
		assertTrue(javascript.contains("type:'text'"));
		assertTrue(javascript.contains("filterEditorType:'text'"));
		assertTrue(javascript.contains("canSave:false"));
		assertTrue(javascript.contains("sortByField:'status'"));
		assertTrue(javascript.contains("validOperators:['iContains','isNull','notNull']"));
	}

	@Test
	void projectedColumnWithDynamicDomainDisablesFiltering() throws Exception {
		Text status = new Text();
		status.setName("status");
		status.setDisplayName("Status");
		status.setDomainType(DomainType.dynamic);

		SmartClientQueryColumnDefinition definition = definitionFor(status);

		assertTrue(definition.getHasTextFilterOperators());
		assertFalse(definition.isCanFilter());
		assertFalse(definition.isCanSave());
		String javascript = definition.toJavascript();
		assertTrue(javascript.contains("type:'text'"));
		assertTrue(javascript.contains("canFilter:false"));
		assertTrue(javascript.contains("canSave:false"));
		assertFalse(javascript.contains("validOperators:['iContains','isNull','notNull']"));
	}

	@Test
	void projectedColumnWithEnumerationDomainUsesEqualsOperators() throws Exception {
		Enumeration choice = new Enumeration();
		choice.setName("choice");
		choice.setDisplayName("Choice");
		choice.setDomainType(DomainType.variant);

		SmartClientQueryColumnDefinition definition = definitionFor(choice);

		String javascript = definition.toJavascript();
		assertTrue(javascript.contains("type:'enum'"));
		assertTrue(javascript.contains("validOperators:['equals','notEqual','isNull','notNull']"));
	}

	@Test
	void projectedColumnWithTextLikeAttributeExposesTextFilterOperators() throws Exception {
		Colour colour = new Colour();
		colour.setName("colour");
		colour.setDisplayName("Colour");

		SmartClientQueryColumnDefinition definition = definitionFor(colour);

		assertTrue(definition.getHasTextFilterOperators());
		assertTrue(definition.toJavascript().contains("editorType:'ColorItem'"));
	}

	@Test
	void toJavascriptIncludesRequiredAndGeometryOperatorsWhenConfigured() {
		MetaDataQueryProjectedColumn column = mock(MetaDataQueryProjectedColumn.class);
		when(column.getBinding()).thenReturn(null);
		when(column.getName()).thenReturn("geom");
		when(column.getLocalisedDisplayName()).thenReturn("Geometry");
		when(column.getAlignment()).thenReturn(null);
		when(column.getPixelWidth()).thenReturn(null);
		when(column.isEscape()).thenReturn(false);
		when(column.isHidden()).thenReturn(false);
		when(column.getFormatterName()).thenReturn(null);
		when(column.getCustomFormatterName()).thenReturn(null);
		when(column.isFilterable()).thenReturn(true);
		when(column.isSortable()).thenReturn(true);
		when(column.isEditable()).thenReturn(true);

		SmartClientQueryColumnDefinition definition = new SmartClientQueryColumnDefinition(null,
				null,
				null,
				null,
				column,
				false,
				null);
		definition.setType("geometry");
		definition.setRequiredMessage("Required geometry");

		String javascript = definition.toJavascript();
		assertTrue(javascript.contains("bizRequired:true"));
		assertTrue(javascript.contains("requiredMessage:'Required geometry'"));
		assertTrue(javascript.contains("validOperators:isc.GeometryItem.validOperators"));
	}

	@Test
	void settersControlQueryColumnFlagsAndContentFallbacks() {
		MetaDataQueryProjectedColumn column = mock(MetaDataQueryProjectedColumn.class);
		when(column.getBinding()).thenReturn(null);
		when(column.getName()).thenReturn("status");
		when(column.getLocalisedDisplayName()).thenReturn("Status");
		when(column.getAlignment()).thenReturn(null);
		when(column.getPixelWidth()).thenReturn(null);
		when(column.isEscape()).thenReturn(false);
		when(column.isHidden()).thenReturn(false);
		when(column.getFormatterName()).thenReturn(null);
		when(column.getCustomFormatterName()).thenReturn(null);
		when(column.isFilterable()).thenReturn(true);
		when(column.isSortable()).thenReturn(true);
		when(column.isEditable()).thenReturn(true);

		SmartClientQueryColumnDefinition definition = new SmartClientQueryColumnDefinition(null,
				null,
				null,
				null,
				column,
				false,
				null);

		definition.setCanFilter(false);
		definition.setCanSave(false);
		definition.setCanSortClientOnly(true);
		definition.setDetail(true);
		definition.setPixelHeight(Integer.valueOf(44));
		definition.setEmptyThumbnailRelativeFile("images/empty.png");

		assertFalse(definition.isCanFilter());
		assertFalse(definition.isCanSave());
		assertTrue(definition.isCanSortClientOnly());
		assertTrue(definition.isDetail());
		assertEquals(Integer.valueOf(44), definition.getPixelHeight());
		assertEquals("images/empty.png", definition.getEmptyThumbnailRelativeFile());
		assertFalse(definition.getHasTextFilterOperators());

		String javascript = definition.toJavascript();
		assertTrue(javascript.contains("canFilter:false"));
		assertTrue(javascript.contains("canSave:false"));
		assertTrue(javascript.contains("detail:true"));
		assertTrue(javascript.contains("canSortClientOnly:true"));
	}

	private static SmartClientQueryColumnDefinition definitionFor(Attribute attribute) throws Exception {
		User user = mock(User.class);
		CustomerImpl customer = org.mockito.Mockito.spy(new CustomerImpl());
		Module module = mock(Module.class);
		DocumentImpl document = new DocumentImpl();
		document.setName("Contact");
		document.setOwningModuleName("sales");
		if (attribute instanceof Enumeration enumeration) {
			enumeration.setOwningDocument(document);
		}
		document.putAttribute(attribute);

		doReturn(module).when(customer).getModule("sales");
		when(module.getDocument(customer, "Contact")).thenReturn(document);
		doReturn(List.of(new DomainValue("A", "Alpha"), new DomainValue("B", "Beta")))
				.when(customer).getConstantDomainValues(null, "sales", "Contact", attribute);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.getBizlet(customer, document, false)).thenReturn(null);
		when(repository.getMetaDataBizlet(customer, document)).thenReturn(null);

		Customisations customisations = mock(Customisations.class);
		when(customisations.determineDefaultColumnTextAlignment("desktop", attribute.getAttributeType()))
				.thenReturn(HorizontalAlignment.left);
		when(customisations.determineDefaultColumnWidth("desktop", attribute.getAttributeType()))
				.thenReturn(Integer.valueOf(100));

		MetaDataQueryProjectedColumn column = mock(MetaDataQueryProjectedColumn.class);
		when(column.getBinding()).thenReturn(attribute.getName());
		when(column.getName()).thenReturn(attribute.getName());
		when(column.getLocalisedDisplayName()).thenReturn(null);
		when(column.getAlignment()).thenReturn(null);
		when(column.getPixelWidth()).thenReturn(null);
		when(column.isEscape()).thenReturn(true);
		when(column.isHidden()).thenReturn(false);
		when(column.getFormatterName()).thenReturn(null);
		when(column.getCustomFormatterName()).thenReturn(null);
		when(column.isFilterable()).thenReturn(true);
		when(column.isSortable()).thenReturn(true);
		when(column.isEditable()).thenReturn(true);

		Customisations previousCustomisations = CustomisationsStaticSingleton.get();
		boolean hadRepository = ProvidedRepositoryFactory.isConfigured();
		ProvidedRepository previousRepository = hadRepository ? ProvidedRepositoryFactory.get() : null;
		try {
			CustomisationsStaticSingleton.set(customisations);
			ProvidedRepositoryFactory.set(repository);
			return new SmartClientQueryColumnDefinition(user, customer, module, document, column, false, "desktop");
		}
		finally {
			CustomisationsStaticSingleton.set(previousCustomisations);
			if (hadRepository) {
				ProvidedRepositoryFactory.set(Objects.requireNonNull(previousRepository));
			}
			else {
				ProvidedRepositoryFactory.clear();
			}
		}
	}
}
