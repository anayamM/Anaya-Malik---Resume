"""
CS Final Project
  File: Project2.py
  Description: The project develops a recommendation 
  system for Coursera courses that allows users 
  to input a keyword related to the course 
  they are looking for.
  Student Name: Ridisha Saquib
  Student UT EID: rns2365
  Partner Name: Anaya Malik
  Partner UT EID: ahm2466
  Course Name: CS 313E
  Unique Number: 50165
  Date Created: 11/21/2024
  Date Last Modified: 11/21/2024
"""
import pandas as pd

csv_file_path = 'Coursera.csv'

try:
    with open(csv_file_path, 'r') as f:
        print(f"File found: {csv_file_path}")
except FileNotFoundError:
    print(f"File not found: {csv_file_path}")
    exit()

def load_in_data(file_path):
    """
    Loads in the data, prcoesses it,
    filters the data with ratings 4.0
    or higher. Converts 'Course Rating' 
    to numeric, replacing invalid values 
    with NaN, and removes rows with NaN 
    before filtering.
    """
    df = pd.read_csv(file_path)
    print("Columns in the DataFrame:", df.columns)
    df['Course Rating'] = pd.to_numeric(df['Course Rating'], errors='coerce')
    df = df.dropna(subset=['Course Rating'])
    return df[df['Course Rating'] >= 4.0]

class MaxHeap:
    """
    Implements a max-heap data structure
    that stores courses based of ratings, 
    ensuring highest rated courses are
    accessible.
    """
    def __init__(self):
        """
        Initializer for class.
        """
        self.heap = []

    def insert(self, course_name, course_rating, course_difficulty):
        """
       The heap is updated
        by inserting a new course and
        reorganized for max-heap 
        property.
        """
        self.heap.append((course_rating, course_name, course_difficulty))
        self._heapify_up(len(self.heap) - 1)

    def _heapify_up(self, index):
        """
        Ensures the max-heap property by
        swapping elements in the heap and
        moving the index of element up
        the heap.
        """
        while index > 0:
            parent_index = (index - 1) // 2
            if self.heap[index][0] > self.heap[parent_index][0]:
                self.heap[index], self.heap[parent_index] = self.heap[parent_index], self.heap[index]
                index = parent_index
            else:
                break

    def extract_max(self):
        """
        The maximum element is removed
        and returned.If the heap is 
        empty, none is returned.
        """
        if len(self.heap) == 0:
            return None
        if len(self.heap) == 1:
            return self.heap.pop()
        max_element = self.heap[0]
        self.heap[0] = self.heap.pop()
        self._heapify_down(0)
        return max_element

    def _heapify_down(self, index):
        """
        Ensure the max-heap property by moving
        the element at the index down the heap.
        It reorganizes the heap by comparing
        the element with its children and 
        swapping as needed.
        """
        while True:
            left_child = 2 * index + 1
            right_child = 2 * index + 2
            largest = index

            if left_child < len(self.heap) and self.heap[left_child][0] > self.heap[largest][0]:
                largest = left_child
            if right_child < len(self.heap) and self.heap[right_child][0] > self.heap[largest][0]:
                largest = right_child
            if largest == index:
                break
            self.heap[index], self.heap[largest] = self.heap[largest], self.heap[index]
            index = largest

def filter_data(df, keyword):
    """
    Filters courses from DataFrame based
    on a keyword.Returns a dictionary where
    the keys are course names and the values
    are tuples of course rating and course 
    difficulty.
    """
    filtered_courses = {}
    for _, row in df.iterrows():
        course_name = row['Course Name']
        course_rating = row['Course Rating']
        course_difficulty = row['Difficulty Level']
        if keyword.lower() in course_name.lower():
            filtered_courses[course_name] = (course_rating, course_difficulty)
    return filtered_courses

def search_courses(courses):
    """
    Ranks courses by ratings using a 
    max heap and returns a list of courses
    in descending order. Courses with the
    highest rating are extracted first.
    """
    heap = MaxHeap()
    for course_name, (course_rating, course_difficulty) in courses.items():
        heap.insert(course_name, course_rating, course_difficulty)
    sorted_courses = []
    while True:
        max_course = heap.extract_max()
        if not max_course:
            break
        sorted_courses.append(max_course)
    return sorted_courses

def display_top_courses(sorted_courses, top_n=15):
    """
    Display top-rated courses with difficulty levels.
    """
    print(f"\nTop {top_n} Courses:")
    for i, (rating, title, difficulty) in enumerate(sorted_courses[:top_n], 1):
        print(f"{i}. {title} - Rating: {rating}, Difficulty: {difficulty}")

def test_load_in_data():
    """
    Check if dataset is loaded correctly from 
    the CSV file and if it has the expected columns.
    """
    df = load_in_data(csv_file_path)
    assert 'Course Name' in df.columns, "Missing 'Course Name' column."
    assert 'Course Rating' in df.columns, "Missing 'Course Rating' column."
    assert len(df) > 0, "The dataset appears to be empty."
    print("test_load_in_data passed!")

def test_filter_by_rating():
    """
    Verifies that the filtering function 
    correctly removes courses with 
    a rating lower than a 4.0.
    """
    df = load_in_data(csv_file_path)
    keyword = "Python"  # Using an example keyword for filtering
    filtered_courses = filter_data(df, keyword)
    for course_name, (course_rating, _) in filtered_courses.items():
        assert course_rating >= 4.0, f"Course '{course_name}' has a rating below 4.0."
    print("test_filter_by_rating passed!")

def test_keyword_search():
    """
     Ensure the courses containing the 
     search keyword in the description 
     are correctly identified.
    """
    df = load_in_data(csv_file_path)
    keyword = "Python"
    filtered_courses = filter_data(df, keyword)
    for course_name in filtered_courses.keys():
        assert keyword.lower() in course_name.lower(), f"Keyword '{keyword}' not found in course name: {course_name}."
    print("test_keyword_search passed!")

def main():
    """
    The main function to execute the
    course reccomendation system.
    Executes test cases and prints 
    results. Prompts user for input.
    Displays the top courses or message
    if not found.
    """
    test_load_in_data()
    test_filter_by_rating()
    test_keyword_search()
    if not csv_file_path:
        return
    df = load_in_data(csv_file_path)
    keyword = input("\nEnter a keyword to search for related courses: ")
    filtered_courses = filter_data(df, keyword)
    if filtered_courses:
        sorted_courses = search_courses(filtered_courses)
        display_top_courses(sorted_courses, top_n=15)
    else:
        print("No courses found matching that keyword.")

if __name__ == "__main__":
    main()
