package com.drekitek;

import org.skyve.impl.web.service.smartclient.SmartClientViewRenderer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;

public class DrekitekSmartClientViewRenderer extends SmartClientViewRenderer {
	public DrekitekSmartClientViewRenderer(User user,
												Module module,
												Document document,
												View view,
												String uxui,
												boolean noCreateView) {
		super(user, module, document, view, uxui, noCreateView);
	}

	@Override
	public void renderedView(String icon16x16Url, String icon32x32Url) {
		super.renderedView(icon16x16Url, icon32x32Url);
		StringBuilder code = getCode();
		int i = 0, l = code.length();
		while ((i >= 0) && (i < l)) {
			i = code.indexOf("isc.BizListGrid", i);
			if (i >= 0) {
				code.replace(i, i + 15, "isc.DrekListGrid");
			}
		}
	}
}
