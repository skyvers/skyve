package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.FormatterName;
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;

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
}