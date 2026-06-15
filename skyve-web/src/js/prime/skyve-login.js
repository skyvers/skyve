if (!window.SKYVE) {
	window.SKYVE = {};
}

SKYVE.Login = (function() {
	function syncTwoFactorCode() {
		var tfaInputs = document.querySelectorAll(".js-tfa-code");
		if (! tfaInputs.length) {
			return;
		}

		var hiddenPassword = document.getElementById("password");
		var code = "";
		for (var i = 0; i < tfaInputs.length; i++) {
			code += tfaInputs[i].value;
		}
		hiddenPassword.value = code;
	}

	function initialiseTwoFactorCodeInputs() {
		var tfaInputs = document.querySelectorAll(".js-tfa-code");
		if (! tfaInputs.length) {
			return;
		}

		function focusFirstEmptyInput() {
			for (var i = 0; i < tfaInputs.length; i++) {
				if (tfaInputs[i].value === "") {
					tfaInputs[i].focus();
					return;
				}
			}
			tfaInputs[tfaInputs.length - 1].focus();
		}

		function fillFrom(index, rawValue) {
			var digits = rawValue.replace(/\D/g, "");
			if (! digits) {
				return;
			}

			for (var i = 0; (i < digits.length) && ((index + i) < tfaInputs.length); i++) {
				tfaInputs[index + i].value = digits.charAt(i);
			}
			syncTwoFactorCode();
			focusFirstEmptyInput();
		}

		for (var i = 0; i < tfaInputs.length; i++) {
			(function(index) {
				var input = tfaInputs[index];

				input.addEventListener("input", function() {
					var value = input.value.replace(/\D/g, "");
					if (value.length <= 1) {
						input.value = value;
						syncTwoFactorCode();
						if (value && (index < (tfaInputs.length - 1))) {
							tfaInputs[index + 1].focus();
							tfaInputs[index + 1].select();
						}
						return;
					}
					fillFrom(index, value);
				});

				input.addEventListener("keydown", function(event) {
					if ((event.key === "Backspace") && (! input.value) && (index > 0)) {
						tfaInputs[index - 1].value = "";
						tfaInputs[index - 1].focus();
						syncTwoFactorCode();
						event.preventDefault();
					}
					else if ((event.key === "ArrowLeft") && (index > 0)) {
						tfaInputs[index - 1].focus();
						event.preventDefault();
					}
					else if ((event.key === "ArrowRight") && (index < (tfaInputs.length - 1))) {
						tfaInputs[index + 1].focus();
						event.preventDefault();
					}
				});

				input.addEventListener("paste", function(event) {
					var clipboard = event.clipboardData || window.clipboardData;
					var pasted = clipboard ? clipboard.getData("text") : "";
					if (! pasted) {
						return;
					}
					event.preventDefault();
					fillFrom(index, pasted);
				});

				input.addEventListener("focus", function() {
					input.select();
				});
			})(i);
		}

		focusFirstEmptyInput();
		syncTwoFactorCode();
	}

	return {
		syncTwoFactorCode: syncTwoFactorCode,
		initialiseTwoFactorCodeInputs: initialiseTwoFactorCodeInputs
	};
})();
