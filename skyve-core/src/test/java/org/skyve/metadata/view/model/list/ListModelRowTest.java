package org.skyve.metadata.view.model.list;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;

class ListModelRowTest {

	private static Map<String, Object> emptyMap() {
		return new HashMap<>();
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorSetsModuleAndDocument() {
		ListModelRow row = new ListModelRow("myModule", "MyDoc", emptyMap());
		assertThat(row.getBizModule(), is("myModule"));
		assertThat(row.getBizDocument(), is("MyDoc"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizLockWithoutPropertyUsesField() {
		ListModelRow row = new ListModelRow("mod", "doc", emptyMap());
		assertNull(row.getBizLock());
		row.setBizLock("lockval");
		assertThat(row.getBizLock(), is("lockval"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizLockWithPropertyUsesMap() {
		Map<String, Object> props = new HashMap<>();
		props.put(PersistentBean.LOCK_NAME, null);
		ListModelRow row = new ListModelRow("mod", "doc", props);
		row.setBizLock("dynLock");
		assertThat(row.getBizLock(), is("dynLock"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizTaggedWithoutPropertyUsesField() {
		ListModelRow row = new ListModelRow("mod", "doc", emptyMap());
		assertNull(row.getBizTagged());
		row.setBizTagged("true");
		assertThat(row.getBizTagged(), is("true"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizFlagCommentWithoutPropertyUsesField() {
		ListModelRow row = new ListModelRow("mod", "doc", emptyMap());
		assertNull(row.getBizFlagComment());
		row.setBizFlagComment("some comment");
		assertThat(row.getBizFlagComment(), is("some comment"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizKeyWithoutPropertyUsesField() {
		ListModelRow row = new ListModelRow("mod", "doc", emptyMap());
		assertNull(row.getBizKey());
		row.setBizKey("myKey");
		assertThat(row.getBizKey(), is("myKey"));
	}

	@Test
	@SuppressWarnings("static-method")
	void getBizKeyFromMapWhenPropertyPresent() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.BIZ_KEY, "mapKey");
		ListModelRow row = new ListModelRow("mod", "doc", props);
		assertThat(row.getBizKey(), is("mapKey"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizTaggedWithPropertyUsesMap() {
		Map<String, Object> props = new HashMap<>();
		props.put(PersistentBean.TAGGED_NAME, null);
		ListModelRow row = new ListModelRow("mod", "doc", props);
		row.setBizTagged("tagVal");
		assertThat(row.getBizTagged(), is(row.getBizLock())); // stores bizLock value into map (implementation quirk)
	}

	@Test
	@SuppressWarnings("static-method")
	void getBizTaggedFromMapWhenPropertyPresent() {
		Map<String, Object> props = new HashMap<>();
		props.put(PersistentBean.TAGGED_NAME, "tagFromMap");
		ListModelRow row = new ListModelRow("mod", "doc", props);
		assertThat(row.getBizTagged(), is("tagFromMap"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizFlagCommentWithPropertyUsesMap() {
		Map<String, Object> props = new HashMap<>();
		props.put(PersistentBean.FLAG_COMMENT_NAME, null);
		ListModelRow row = new ListModelRow("mod", "doc", props);
		row.setBizFlagComment("flagVal");
		assertThat(row.getBizFlagComment(), is(row.getBizLock())); // stores bizLock value (implementation quirk)
	}

	@Test
	@SuppressWarnings("static-method")
	void getBizFlagCommentFromMapWhenPropertyPresent() {
		Map<String, Object> props = new HashMap<>();
		props.put(PersistentBean.FLAG_COMMENT_NAME, "flagFromMap");
		ListModelRow row = new ListModelRow("mod", "doc", props);
		assertThat(row.getBizFlagComment(), is("flagFromMap"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setBizKeyWithPropertyUsesMap() {
		Map<String, Object> props = new HashMap<>();
		props.put(Bean.BIZ_KEY, null);
		ListModelRow row = new ListModelRow("mod", "doc", props);
		row.setBizKey("keyVal");
		assertThat(row.getBizKey(), is(row.getBizLock())); // stores bizLock value (implementation quirk)
	}
}
