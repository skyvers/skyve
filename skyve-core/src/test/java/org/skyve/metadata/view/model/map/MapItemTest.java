package org.skyve.metadata.view.model.map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;

class MapItemTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorInitialisesFeatureList() {
		MapItem item = new MapItem();
		assertNotNull(item.getFeatures());
		assertEquals(0, item.getFeatures().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizIdRoundtrip() {
		MapItem item = new MapItem();
		item.setBizId("id-123");
		assertThat(item.getBizId(), is("id-123"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setModuleNameRoundtrip() {
		MapItem item = new MapItem();
		item.setModuleName("admin");
		assertThat(item.getModuleName(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setDocumentNameRoundtrip() {
		MapItem item = new MapItem();
		item.setDocumentName("User");
		assertThat(item.getDocumentName(), is("User"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setInfoMarkupRoundtrip() {
		MapItem item = new MapItem();
		item.setInfoMarkup("<b>Info</b>");
		assertThat(item.getInfoMarkup(), is("<b>Info</b>"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setFromTimestampRoundtrip() {
		MapItem item = new MapItem();
		Timestamp ts = new Timestamp();
		item.setFromTimestamp(ts);
		assertThat(item.getFromTimestamp(), is(ts));
	}

	@Test
	@SuppressWarnings("static-method")
	void setToTimestampRoundtrip() {
		MapItem item = new MapItem();
		Timestamp ts = new Timestamp();
		item.setToTimestamp(ts);
		assertThat(item.getToTimestamp(), is(ts));
	}

	@Test
	@SuppressWarnings("static-method")
	void addFeatureIsReflectedInList() {
		MapItem item = new MapItem();
		MapFeature feature = new MapFeature();
		item.getFeatures().add(feature);
		assertEquals(1, item.getFeatures().size());
	}
}
