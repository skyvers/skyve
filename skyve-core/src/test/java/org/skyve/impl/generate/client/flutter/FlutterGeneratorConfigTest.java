package org.skyve.impl.generate.client.flutter;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;

import org.junit.jupiter.api.Test;
import org.skyve.impl.generate.client.flutter.FlutterGenerator.GeneratorConfig;
import org.skyve.impl.generate.client.flutter.FlutterGenerator.MoDoc;

class FlutterGeneratorConfigTest {

	// ─── MoDoc ────────────────────────────────────────────────────────────────

	@Test
	@SuppressWarnings("static-method")
	void modocConstructorSetsModuleAndDocument() {
		MoDoc md = new MoDoc("admin", "Contact");
		assertEquals("admin", md.getModule());
		assertEquals("Contact", md.getDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void modocFromStringParsesModuleAndDocument() {
		MoDoc md = MoDoc.fromString("admin.Contact");
		assertEquals("admin", md.getModule());
		assertEquals("Contact", md.getDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void modocMatchesExactModuleAndDocument() {
		MoDoc md = new MoDoc("admin", "Contact");
		assertTrue(md.matches("admin", "Contact"));
	}

	@Test
	@SuppressWarnings("static-method")
	void modocDoesNotMatchDifferentDocument() {
		MoDoc md = new MoDoc("admin", "Contact");
		assertFalse(md.matches("admin", "Group"));
	}

	@Test
	@SuppressWarnings("static-method")
	void modocDoesNotMatchDifferentModule() {
		MoDoc md = new MoDoc("admin", "Contact");
		assertFalse(md.matches("cms", "Contact"));
	}

	@Test
	@SuppressWarnings("static-method")
	void modocWildcardModuleMatchesAnyModule() {
		MoDoc md = new MoDoc(MoDoc.WILDCARD, "Contact");
		assertTrue(md.matches("admin", "Contact"));
		assertTrue(md.matches("cms", "Contact"));
		assertFalse(md.matches("admin", "Group"));
	}

	@Test
	@SuppressWarnings("static-method")
	void modocWildcardDocumentMatchesAnyDocument() {
		MoDoc md = new MoDoc("admin", MoDoc.WILDCARD);
		assertTrue(md.matches("admin", "Contact"));
		assertTrue(md.matches("admin", "Group"));
		assertFalse(md.matches("cms", "Contact"));
	}

	@Test
	@SuppressWarnings("static-method")
	void modocBothWildcardsMatchAnything() {
		MoDoc md = new MoDoc(MoDoc.WILDCARD, MoDoc.WILDCARD);
		assertTrue(md.matches("admin", "Contact"));
		assertTrue(md.matches("cms", "Group"));
	}

	@Test
	@SuppressWarnings("static-method")
	void modocToStringFormat() {
		MoDoc md = new MoDoc("admin", "Contact");
		assertEquals("MoDoc[admin.Contact]", md.toString());
	}

	// ─── GeneratorConfig ──────────────────────────────────────────────────────

	@Test
	@SuppressWarnings("static-method")
	void generatorConfigDefaultWhitelistIsEmpty() {
		GeneratorConfig cfg = new GeneratorConfig();
		assertNotNull(cfg.getModocWhitelist());
		assertTrue(cfg.getModocWhitelist().isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void generatorConfigDefaultCustomerIsNull() {
		assertThat(new GeneratorConfig().getCustomer(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void generatorConfigSetAndGetUxui() {
		GeneratorConfig cfg = new GeneratorConfig();
		cfg.setUxui("desktop");
		assertEquals("desktop", cfg.getUxui());
	}

	@Test
	@SuppressWarnings("static-method")
	void generatorConfigSetAndGetProjectName() {
		GeneratorConfig cfg = new GeneratorConfig();
		cfg.setProjectName("MyApp");
		assertEquals("MyApp", cfg.getProjectName());
	}

	@Test
	@SuppressWarnings("static-method")
	void generatorConfigSetAndGetProjectPath() {
		GeneratorConfig cfg = new GeneratorConfig();
		cfg.setProjectPath("/some/path");
		assertEquals("/some/path", cfg.getProjectPath());
	}

	@Test
	@SuppressWarnings("static-method")
	void generatorConfigSetAndGetCustomerName() {
		GeneratorConfig cfg = new GeneratorConfig();
		cfg.setCustomerName("acme");
		assertEquals("acme", cfg.getCustomerName());
	}

	@Test
	@SuppressWarnings("static-method")
	void generatorConfigSetAndGetLibViewsPath() {
		GeneratorConfig cfg = new GeneratorConfig();
		File f = new File("/tmp");
		cfg.setLibViewsPath(f);
		assertEquals(f, cfg.getLibViewsPath());
	}

	@Test
	@SuppressWarnings("static-method")
	void generatorConfigAddModocWhitelistEntryIncreasesSize() {
		GeneratorConfig cfg = new GeneratorConfig();
		cfg.addModocWhitelistEntry("admin.Contact");
		assertEquals(1, cfg.getModocWhitelist().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void generatorConfigAllowsMoDocMatchesAddedEntry() {
		GeneratorConfig cfg = new GeneratorConfig();
		cfg.addModocWhitelistEntry("admin.Contact");
		assertTrue(cfg.allowsMoDoc("admin", "Contact"));
		assertFalse(cfg.allowsMoDoc("admin", "Group"));
	}

	@Test
	@SuppressWarnings("static-method")
	void generatorConfigAllowsMoDocWithWildcardModule() {
		GeneratorConfig cfg = new GeneratorConfig();
		cfg.addModocWhitelistEntry("*.Contact");
		assertTrue(cfg.allowsMoDoc("admin", "Contact"));
		assertTrue(cfg.allowsMoDoc("cms", "Contact"));
		assertFalse(cfg.allowsMoDoc("admin", "Group"));
	}

	@Test
	@SuppressWarnings("static-method")
	void generatorConfigAllowsMoDocEmptyWhitelistReturnsFalse() {
		GeneratorConfig cfg = new GeneratorConfig();
		assertFalse(cfg.allowsMoDoc("admin", "Contact"));
	}
}
