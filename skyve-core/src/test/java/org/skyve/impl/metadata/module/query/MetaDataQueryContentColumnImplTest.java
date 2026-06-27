package org.skyve.impl.metadata.module.query;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.metadata.view.TextOutput.Sanitisation;

class MetaDataQueryContentColumnImplTest {
	@Test
	@SuppressWarnings("static-method")
	void defaultDisplayIsNull() {
		MetaDataQueryContentColumnImpl col = new MetaDataQueryContentColumnImpl();
		assertEquals(null, col.getDisplay());
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetDisplay() {
		MetaDataQueryContentColumnImpl col = new MetaDataQueryContentColumnImpl();
		col.setDisplay(DisplayType.thumbnail);
		assertEquals(DisplayType.thumbnail, col.getDisplay());
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetPixelHeight() {
		MetaDataQueryContentColumnImpl col = new MetaDataQueryContentColumnImpl();
		col.setPixelHeight(Integer.valueOf(200));
		assertEquals(Integer.valueOf(200), col.getPixelHeight());
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetEmptyThumbnailRelativeFile() {
		MetaDataQueryContentColumnImpl col = new MetaDataQueryContentColumnImpl();
		col.setEmptyThumbnailRelativeFile("images/empty.png");
		assertEquals("images/empty.png", col.getEmptyThumbnailRelativeFile());
	}

	@Test
	@SuppressWarnings("static-method")
	void getPropertiesIsNotNull() {
		MetaDataQueryContentColumnImpl col = new MetaDataQueryContentColumnImpl();
		assertNotNull(col.getProperties());
	}

	@Test
	@SuppressWarnings("static-method")
	void isEscapeReturnsFalse() {
		assertFalse(new MetaDataQueryContentColumnImpl().isEscape());
	}

	@Test
	@SuppressWarnings("static-method")
	void getSanitiseReturnsNone() {
		assertEquals(Sanitisation.none, new MetaDataQueryContentColumnImpl().getSanitise());
	}
}
