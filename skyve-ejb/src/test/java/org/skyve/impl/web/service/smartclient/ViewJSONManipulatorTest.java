package org.skyve.impl.web.service.smartclient;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.view.View;
import org.skyve.util.Util;

import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;

public class ViewJSONManipulatorTest extends AbstractSkyveTest {
	@Test
	public void testAllFormatsVisible()
			throws Exception {
		View view = createView();

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = new SmartClientWebContext("key", null, null);
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		Assert.assertTrue("There should be 2 formats in the payload - " + json,
				json.contains("_0") && json.contains("_1"));
	}

	@Test
	public void testFirstFormatVisible()
			throws Exception {
		ViewImpl view = createView();
		Blurb b2 = (Blurb) ((Form) view.getContained().get(0)).getRows().get(1).getItems().get(0).getWidget();
		b2.setInvisibleConditionName("true");

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = new SmartClientWebContext("key", null, null);
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		Assert.assertTrue("There should be 2 formats in the payload - " + json,
				json.contains("_0") && (!json.contains("_1")));
	}

	@Test
	public void testSecondFormatVisible()
			throws Exception {
		ViewImpl view = createView();
		Blurb b1 = (Blurb) ((Form) view.getContained().get(0)).getRows().get(0).getItems().get(0).getWidget();
		b1.setInvisibleConditionName("true");

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = new SmartClientWebContext("key", null, null);
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		Assert.assertTrue("There should be 2 formats in the payload - " + json,
				(!json.contains("_0")) && json.contains("_1"));
	}

	@Test
	public void testFirstFormFormatsInvisible()
			throws Exception {
		ViewImpl view = createView();
		((Form) view.getContained().get(0)).setInvisibleConditionName("true");
		view.getContained().add(createForm());

		AllAttributesPersistent bean = Util.constructRandomInstance(u, m, aapd, 0);
		ViewJSONManipulator vjm = new ViewJSONManipulator(u, m, aapd, view, bean, 0, 0, false);
		vjm.visit();
		AbstractWebContext ctx = new SmartClientWebContext("key", null, null);
		ctx.setCurrentBean(bean);
		String json = vjm.toJSON(ctx, null);
		Assert.assertTrue("There should be 2 formats in the payload - " + json,
				(!json.contains("_0")) && (!json.contains("_1") &&
						json.contains("_2") && json.contains("_3")));
	}

	private static ViewImpl createView() {
		ViewImpl result = new ViewImpl();
		result.setTitle("TEST");

		result.getContained().add(createForm());

		return result;
	}

	private static Form createForm() {
		Form f = new Form();
		f.getColumns().add(new FormColumn());
		f.getColumns().add(new FormColumn());

		FormRow r = new FormRow();
		FormItem i = new FormItem();
		Blurb b = new Blurb();
		b.setMarkup("Test {bizKey}");
		i.setWidget(b);
		r.getItems().add(i);
		f.getRows().add(r);

		r = new FormRow();
		i = new FormItem();
		b = new Blurb();
		b.setMarkup("Test {bizId}");
		i.setWidget(b);
		r.getItems().add(i);
		f.getRows().add(r);

		return f;
	}
}
