let dropArea = document.getElementById('drop-area');
let imageInput = document.getElementById('image-input');
let extractForm = document.getElementById('extract-form');

const dragClasses = ['border-gray-500', 'border-2', 'bg-gray-100'];

dropArea.addEventListener('dragover', function (e) {
	dragClasses.forEach(className => {
		this.classList.add(className);
	});
	e.preventDefault();
});

dropArea.addEventListener('dragleave', function (e) {
	dragClasses.forEach(className => {
		this.classList.remove(className);
	});
	e.preventDefault();
});

dropArea.addEventListener('drop', function (e) {
	dragClasses.forEach(className => {
		this.classList.remove(className);
	});

	imageInput.files = e.dataTransfer.files;
	extractForm.submit();

	e.preventDefault();
	e.stopPropagation();
});
