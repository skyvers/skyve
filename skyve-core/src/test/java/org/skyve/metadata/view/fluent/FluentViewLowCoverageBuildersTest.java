package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.InjectBinding;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.reference.ExternalReference;
import org.skyve.impl.metadata.view.reference.Reference;
import org.skyve.impl.metadata.view.reference.ReferenceTarget;
import org.skyve.impl.metadata.view.reference.ReferenceTarget.ReferenceTargetType;
import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.View.ViewParameter;

@SuppressWarnings("static-method")
class FluentViewLowCoverageBuildersTest {

	@Test
	void linkAndTextStyleBuildersMapFromAndSetters() {
		Link link = new Link();
		Reference reference = new ExternalReference();
		ReferenceTarget target = new ReferenceTarget();
		target.setType(ReferenceTargetType.blankFrame);
		link.setReference(reference);
		link.setTarget(target);
		link.setValue("Open");
		link.setInvisibleConditionName("hideLink");
		link.setPixelWidth(Integer.valueOf(140));
		FluentLink fluentLink = new FluentLink().from(link).value("Updated").pixelWidth(180);
		assertThat(fluentLink.get().getReference(), is(reference));
		assertThat(fluentLink.get().getTarget(), is(target));
		assertThat(fluentLink.get().getValue(), is("Updated"));
		assertThat(fluentLink.get().getPixelWidth(), is(Integer.valueOf(180)));

		RichText richText = new RichText();
		richText.setSanitise(Sanitisation.text);
		FluentRichText fluentRichText = new FluentRichText().from(richText).sanitise(Sanitisation.relaxed).pixelWidth(400).pixelHeight(200);
		fluentRichText.minPixelHeight(120).maxPixelHeight(500);
		assertThat(fluentRichText.get().getSanitise(), is(Sanitisation.relaxed));
		assertThat(fluentRichText.get().getPixelWidth(), is(Integer.valueOf(400)));

		HTML html = new HTML();
		html.setSanitise(Sanitisation.basic);
		FluentHTML fluentHtml = new FluentHTML().from(html).sanitise(Sanitisation.none).pixelWidth(320).pixelHeight(160);
		assertThat(fluentHtml.get().getSanitise(), is(Sanitisation.none));
		assertThat(fluentHtml.get().getPixelHeight(), is(Integer.valueOf(160)));
	}

	@Test
	void contentAndBindingBuildersMapFromAndSetters() {
		ContentSignature signature = new ContentSignature();
		signature.setRgbHexBackgroundColour("#ffffff");
		signature.setRgbHexForegroundColour("#000000");
		FluentContentSignature fluentSignature = new FluentContentSignature().from(signature);
		fluentSignature.rgbHexBackgroundColour("#eeeeee").rgbHexForegroundColour("#111111").pixelWidth(220).pixelHeight(100);
		assertThat(fluentSignature.get().getRgbHexBackgroundColour(), is("#eeeeee"));
		assertThat(fluentSignature.get().getRgbHexForegroundColour(), is("#111111"));
		assertThat(fluentSignature.get().getPixelWidth(), is(Integer.valueOf(220)));

		FilterParameterImpl filterParameter = new FilterParameterImpl();
		filterParameter.setFilterBinding("status");
		filterParameter.setValue("ACTIVE");
		filterParameter.setValueBinding("{bean.status}");
		filterParameter.setOperator(FilterOperator.equal);
		FluentFilterParameter fluentFilterParameter = new FluentFilterParameter().from(filterParameter);
		fluentFilterParameter.filterBinding("name").value("Jane").valueBinding("{bean.name}").operator(FilterOperator.like);
		assertThat(fluentFilterParameter.get().getFilterBinding(), is("name"));
		assertThat(fluentFilterParameter.get().getOperator(), is(FilterOperator.like));

		ViewParameter viewParameter = new ViewParameter();
		viewParameter.setFromBinding("fromBinding");
		viewParameter.setBoundTo("boundTo");
		FluentViewParameter fluentViewParameter = new FluentViewParameter().from(viewParameter).fromBinding("fromUpdated").boundTo("toUpdated");
		assertThat(fluentViewParameter.get().getFromBinding(), is("fromUpdated"));
		assertThat(fluentViewParameter.get().getBoundTo(), is("toUpdated"));

		Inject inject = new Inject();
		inject.setScript("return true;");
		inject.getBindings().add(new InjectBinding());
		FluentInject fluentInject = new FluentInject().from(inject).script("return false;");
		fluentInject.addBinding(new FluentInjectBinding().binding("text").readOnly(Boolean.TRUE));
		assertThat(fluentInject.get().getScript(), is("return false;"));
		assertThat(fluentInject.get().getBindings().size(), is(2));
	}

	@Test
	void membershipAndDefaultWidgetBuildersMapFromAndSetters() {
		CheckMembership checkMembership = new CheckMembership();
		checkMembership.setBinding("roles");
		FluentCheckMembership fluentCheckMembership = new FluentCheckMembership().from(checkMembership).binding("updatedRoles");
		assertThat(fluentCheckMembership.get().getBinding(), is("updatedRoles"));

		Password password = new Password();
		password.setBinding("password");
		password.setPixelWidth(Integer.valueOf(160));
		FluentPassword fluentPassword = new FluentPassword().from(password).pixelWidth(240);
		assertThat(fluentPassword.get().getBinding(), is("password"));
		assertThat(fluentPassword.get().getPixelWidth(), is(Integer.valueOf(240)));

		ListMembership listMembership = new ListMembership();
		listMembership.setBinding("groups");
		listMembership.getChangedActions().add(new RerenderEventAction());
		FluentListMembership fluentListMembership = new FluentListMembership().from(listMembership);
		fluentListMembership.addChangedAction(new FluentToggleVisibilityEventAction().binding("groups"));
		assertThat(fluentListMembership.get().getBinding(), is("groups"));
		assertThat(fluentListMembership.get().getChangedActions().size(), is(2));

		DefaultWidget defaultWidget = new DefaultWidget();
		defaultWidget.setBinding("defaultBinding");
		FluentDefaultWidget fluentDefaultWidget = new FluentDefaultWidget().from(defaultWidget).binding("updatedBinding");
		assertThat(fluentDefaultWidget.get().getBinding(), is("updatedBinding"));
	}

	@Test
	void eventActionBuildersMapFromAndDispatchByType() {
		FluentSetDisabledEventAction setDisabled = new FluentSetDisabledEventAction().from(new SetDisabledEventAction())
				.binding("a").disabledConditionName("cond");
		assertThat(setDisabled.get().getBinding(), is("a"));
		assertThat(setDisabled.get().getDisabledConditionName(), is("cond"));

		FluentSetInvisibleEventAction setInvisible = new FluentSetInvisibleEventAction().from(new SetInvisibleEventAction())
				.binding("b").invisibleConditionName("hide");
		assertThat(setInvisible.get().getBinding(), is("b"));
		assertThat(setInvisible.get().getInvisibleConditionName(), is("hide"));

		FluentToggleDisabledEventAction toggleDisabled = new FluentToggleDisabledEventAction().from(new ToggleDisabledEventAction()).binding("c");
		assertThat(toggleDisabled.get().getBinding(), is("c"));

		FluentToggleVisibilityEventAction toggleVisibility = new FluentToggleVisibilityEventAction().from(new ToggleVisibilityEventAction()).binding("d");
		assertThat(toggleVisibility.get().getBinding(), is("d"));

		FluentServerSideActionEventAction serverSide = new FluentServerSideActionEventAction().from(new ServerSideActionEventAction()).actionName("doSave");
		assertThat(serverSide.get().getActionName(), is("doSave"));

		FluentRerenderEventAction rerender = new FluentRerenderEventAction().from(new RerenderEventAction()).clientValidation(false);
		assertThat(rerender.get().getClientValidation(), is(Boolean.FALSE));

		assertThat(FluentEventAction.from(new RerenderEventAction()), is(instanceOf(FluentRerenderEventAction.class)));
		assertThat(FluentEventAction.from(new ServerSideActionEventAction()), is(instanceOf(FluentServerSideActionEventAction.class)));
		assertThat(FluentEventAction.from(new SetDisabledEventAction()), is(instanceOf(FluentSetDisabledEventAction.class)));
		assertThat(FluentEventAction.from(new SetInvisibleEventAction()), is(instanceOf(FluentSetInvisibleEventAction.class)));
		assertThat(FluentEventAction.from(new ToggleDisabledEventAction()), is(instanceOf(FluentToggleDisabledEventAction.class)));
		assertThat(FluentEventAction.from(new ToggleVisibilityEventAction()), is(instanceOf(FluentToggleVisibilityEventAction.class)));

		assertThat(new FluentSetDisabledEventAction().get(), is(notNullValue()));
	}
}
