package modules.test;

import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.UUID;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesPersistent;

public class OWASPTest extends AbstractSkyveTest {

	private final Map<String, String> escape = new TreeMap<>();
	private final Map<String, String> sanitise = new TreeMap<>();
	private final Map<String, String> both = new TreeMap<>();
	
	@BeforeEach
	public void setup() throws Exception {
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
	public void testEscape() throws Exception {
		for (Entry<String, String> entry : escape.entrySet()) {
			Assert.assertEquals("Escape not working", entry.getValue(), OWASP.escapeHtml(entry.getKey()));
		}
	}

	@Test
	public void testSanitise() throws Exception {
		for (Entry<String, String> entry : sanitise.entrySet()) {
			Assert.assertEquals("Sanitise not working", entry.getValue(), OWASP.sanitise(Sanitisation.relaxed, entry.getKey()));
		}
	}

	@Test
	public void testBoth() throws Exception {
		for (Entry<String, String> entry : both.entrySet()) {
			Assert.assertEquals("Sanitise and escape not as expected", entry.getValue(), OWASP.sanitiseAndEscapeHtml(Sanitisation.relaxed, entry.getKey()));
		}
	}

	@Test
	public void testSanitiseAsFunction() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 2);
		aap.setText("Test<script>alert(1)</script>Me");

		Assert.assertEquals("Format Message with sanitise function should remove script tag", 
								"<h1>TestMe</h1>",
								BindUtil.formatMessage("<h1>{text}</h1>", displayName -> OWASP.sanitise(Sanitisation.relaxed, displayName), aap));
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseBindings() throws Exception {
		Assert.assertEquals("Sanitise should leave bindings alone",
								"user.contacts(1234567890).poo[0]",
								OWASP.sanitise(Sanitisation.text, "user.contacts(1234567890).poo[0]"));
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testSanitiseUUIDs() throws Exception {
		String uuid = UUID.randomUUID().toString();
		Assert.assertEquals("Sanitise should leave UUIDs alone",
								uuid,
								OWASP.sanitise(Sanitisation.text, uuid));
	}

}
