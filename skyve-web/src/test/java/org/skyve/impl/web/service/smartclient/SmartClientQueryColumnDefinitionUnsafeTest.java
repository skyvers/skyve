package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.HorizontalAlignment;

@SuppressWarnings("static-method")
class SmartClientQueryColumnDefinitionUnsafeTest {
	@Test
	void toJavascriptIncludesProjectedFlagsSortAndEqualsOperators() throws Exception {
		SmartClientQueryColumnDefinition def = allocate(SmartClientQueryColumnDefinition.class);
		def.name = "status";
		def.title = "Status";
		def.type = "text";
		def.editorType = "select";
		def.filterEditorType = "comboBox";
		def.length = Integer.valueOf(20);
		def.required = true;
		def.requiredMessage = "Status is required";
		def.valueMap = new LinkedHashMap<>();
		def.valueMap.put("A", "Active");
		def.align = HorizontalAlignment.right;
		def.pixelWidth = Integer.valueOf(120);
		def.escape = true;
		def.setCanFilter(false);
		def.setCanSave(false);
		def.setDetail(true);
		def.setCanSortClientOnly(true);
		setPrivate(def, "sortByField", "statusCode");
		setPrivate(def, "onlyEqualsFilterOperators", Boolean.TRUE);

		String js = def.toJavascript();
		assertTrue(js.contains("name:'status'"));
		assertTrue(js.contains("editorType:'select'"));
		assertTrue(js.contains("filterEditorType:'comboBox'"));
		assertTrue(js.contains("length:20"));
		assertTrue(js.contains("bizRequired:true,requiredMessage:'Status is required'"));
		assertTrue(js.contains("valueMap:{"));
		assertTrue(js.contains("canFilter:false"));
		assertTrue(js.contains("canSave:false"));
		assertTrue(js.contains("detail:true"));
		assertTrue(js.contains("canSortClientOnly:true"));
		assertTrue(js.contains("sortByField:'statusCode'"));
		assertTrue(js.contains("align:'right'"));
		assertTrue(js.contains("width:120"));
		assertTrue(js.contains("escapeHTML:true"));
		assertTrue(js.contains("validOperators:['equals','notEqual','isNull','notNull']"));
	}

	@Test
	void toJavascriptIncludesContainsOperatorsMaskAndValidators() throws Exception {
		SmartClientQueryColumnDefinition def = allocate(SmartClientQueryColumnDefinition.class);
		def.name = "name";
		def.title = "Name";
		def.type = "text";
		def.mask = "AAA";
		def.textBoxStyle = "nameStyle";
		def.validation = "{type:'regexp'}";
		setPrivate(def, "onlyContainsFilterOperator", Boolean.TRUE);

		String js = def.toJavascript();
		assertTrue(js.contains("editorProperties:{mask:'AAA',maskSaveLiterals:true,textBoxStyle:'nameStyle '}"));
		assertTrue(js.contains("validators:[{type:'regexp'}]"));
		assertTrue(js.contains("validOperators:['iContains','isNull','notNull']"));
	}

	@Test
	void toJavascriptForGeometryIncludesGeometryOperators() throws Exception {
		SmartClientQueryColumnDefinition def = allocate(SmartClientQueryColumnDefinition.class);
		def.name = "shape";
		def.title = "Shape";
		def.type = "geometry";

		String js = def.toJavascript();
		assertTrue(js.contains("validOperators:isc.GeometryItem.validOperators"));
	}

	@Test
	void toJavascriptForImageUsesPixelHeightFallbackAndThumbnailPlaceholder() throws Exception {
		SmartClientQueryColumnDefinition def = allocate(SmartClientQueryColumnDefinition.class);
		def.name = "photo";
		def.title = "Photo";
		def.type = "image";
		def.pixelWidth = null;
		def.setPixelHeight(Integer.valueOf(50));
		def.setEmptyThumbnailRelativeFile("img/empty.png");

		String js = def.toJavascript();
		assertTrue(js.contains("formatCellValue:function(v,rec,row,col){if(v){"));
		assertTrue(js.contains("_w=50&_h=50"));
		assertTrue(js.contains("resources?_n=img/empty.png"));
		assertTrue(js.contains(",width:58"));
		assertFalse(js.contains("escapeHTML:true"));
	}

	private static void setPrivate(Object target, String fieldName, Object value) throws Exception {
		Field f = target.getClass().getDeclaredField(fieldName);
		f.setAccessible(true);
		f.set(target, value);
	}

	private static <T> T allocate(Class<T> type) throws Exception {
		Class<?> unsafeType = Class.forName("sun.misc.Unsafe");
		Field f = unsafeType.getDeclaredField("theUnsafe");
		f.setAccessible(true);
		Object unsafe = f.get(null);
		Method allocateInstance = unsafeType.getMethod("allocateInstance", Class.class);
		return type.cast(allocateInstance.invoke(unsafe, type));
	}
}
