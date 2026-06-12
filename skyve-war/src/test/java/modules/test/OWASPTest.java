package modules.test;

import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.util.UUIDv7;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesPersistent;

class OWASPTest extends AbstractSkyveTest {

	private final Map<String, String> escape = new TreeMap<>();
	private final Map<String, String> sanitise = new TreeMap<>();
	private final Map<String, String> both = new TreeMap<>();
	
	@BeforeEach
	void setup() {
		escape.put("poo@wee.com", "poo@wee.com");
		escape.put("<h1>test</h1>", "&lt;h1&gt;test&lt;/h1&gt;");
		escape.put("<h1>test<script>alert(1)</script></h1>", "&lt;h1&gt;test&lt;script&gt;alert(1)&lt;/script&gt;&lt;/h1&gt;");

		sanitise.put("poo@wee.com", "poo@wee.com");
		sanitise.put("<h1>test</h1>", "<h1>test</h1>");
		sanitise.put("<h1>test<script>alert(1)</script></h1>", "<h1>test</h1>");

		both.put("poo@wee.com", "poo@wee.com");
		both.put("<h1>test</h1>", "&lt;h1&gt;test&lt;/h1&gt;");
		both.put("<h1>test<script>alert(1)</script></h1>", "&lt;h1&gt;test&lt;/h1&gt;");
	}
	
	@Test
	void testEscape() {
		for (Entry<String, String> entry : escape.entrySet()) {
			Assertions.assertEquals(entry.getValue(), OWASP.escapeHtml(entry.getKey()), "Escape not working");
		}
	}

	@Test
	void testSanitise() {
		for (Entry<String, String> entry : sanitise.entrySet()) {
			Assertions.assertEquals(entry.getValue(), OWASP.sanitise(Sanitisation.relaxed, entry.getKey()), "Sanitise not working");
		}
	}

	@Test
	void testBoth() {
		for (Entry<String, String> entry : both.entrySet()) {
			Assertions.assertEquals(entry.getValue(), OWASP.sanitiseAndEscapeHtml(Sanitisation.relaxed, entry.getKey()), "Sanitise and escape not as expected");
		}
	}

	@Test
	void testSanitiseAsFunction() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		aap.setText("Test<script>alert(1)</script>Me");

		Assertions.assertEquals(
								"<h1>TestMe</h1>",
								BindUtil.formatMessage("<h1>{text}</h1>", displayName -> OWASP.sanitise(Sanitisation.relaxed, displayName), aap), "Format Message with sanitise function should remove script tag");
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testSanitiseBindings() {
		Assertions.assertEquals(
								"user.contacts(1234567890).poo[0]",
								OWASP.sanitise(Sanitisation.text, "user.contacts(1234567890).poo[0]"), "Sanitise should leave bindings alone");
	}
	
	@Test
	@SuppressWarnings("static-method")
	void testSanitiseUUIDs() {
		String uuid = UUID.randomUUID().toString();
		Assertions.assertEquals(
								uuid,
								OWASP.sanitise(Sanitisation.text, uuid), "Sanitise should leave UUIDs alone");
		uuid = UUIDv7.create().toString();
		Assertions.assertEquals(
								uuid,
								OWASP.sanitise(Sanitisation.text, uuid), "Sanitise should leave UUIDs alone");
	}

}
