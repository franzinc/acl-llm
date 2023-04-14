package llm: Lisp functions Large Language Models APIs.
package llm.gpt: Lisp functions specifically for OpenAI GPT.

I. OpenaAI API Key:

You need an OpenAI API key to use this package.
Currently the program looks in the local directory for the file openai-api.key.

II. Function chat:

Use this function for GPT-3 models ada, babbage, davinci.

III. Simple chatbot function

Example:

```lisp
(chat "Hello.")
```
IV. Function ask-chat:

Use this function for GPT-3.5 API.
Model should be one of: gpt3-3.5-turbo, gpt3-3.5-turbo-0301 or gpt4.
text-or-alist can be either a simple string or a transcript in the form of an alist
(role . content) ...) where role alternates between 'user' and 'system'"

The GPT-3.5 and -4 API works a little differently.

You can still pass it a simple string, but the API also supports a prompt in the form of a transcript.  You can send a transcript between “system” and “user” where the last line is a “user” input.  Here is my first pass at writing a Lisp function to call the API.

Examples: 

```lisp
(ask-chat "Hello")
```

```lisp
(ask-chat
' (("user" . "Maine")
  ("system" . "Augusta")
  ("user" . "California")
  ("system" . "Sacramento")
  ("user" . "Pennsylvania")))
```

```lisp
(ask-chat '
(("user" . "Fill in the blank with one possible appropriate verb phrase or preposition: Gravity ________ Justice.")
 ("system" . "is unrelated to")
 ("user" . "Fill in the blank with one possible appropriate verb phrase or preposition: A tall woman ________ a short man.")
 ("system" . "standing beside")
 ("user" . "Fill in the blank with one possible appropriate verb phrase or preposition: Solar energy ________ clean energy.")
 ("system" . "is")
 ("user" . "Fill in the blank with one possible appropriate verb phrase or preposition: Empire State Building ________ Eiffel Tower.")
 ("system" . "is taller than")
 ("user" . "Fill in the blank with one possible appropriate verb phrase or preposition: An excited Jane ________ an apologetic Tom.")))
```


V. Function call-openai:

Generic interface to all openai API functions using do-http-request.

VI. Function ask-json:

Slightly lower level interface to ask openai for a JSON object.
Each prompt should end with a fixed separator to inform the model when the prompt ends and the completion begins.

VII. Function list-openai-models:

Simple Lisp example of calling openai API to list available models.

VIII. Function list-openai-files:

List all files in your openai directory.

IX. Function delete-openai-file:

Delete a file.

X. Function fine-tune:

Start a fine-tuning process.

XI. Function fine-tune-status:

Get the status of fine-tuning processes.

XII. Function fine-tune-report:

Get the report after a fine-tuning process.

XIII. Function  upload-openai-file:

Upload a file to your opeanai user directory.

XIV. Function cancel-fine-tune:

Cancel a running or pending fine-tune

